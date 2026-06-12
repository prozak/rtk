{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}
-- | Clause normalization: from the parsed grammar (the GENERATED AST,
-- 'GP.Grammar') down to the 'NormalGrammar' the code generators consume.
module Normalize(normalizeTopLevelClauses, fillConstructorNames)
    where

import qualified GrammarParser as GP
import GrammarQQ (clause)

import Syntax
import Diagnostics (Diagnostic(..), SourcePos, showSourcePos)
import Frontend (altElems, clausePos, grammarImports, grammarName, grammarRules,
                 nameText, namePos, ruleClause, ruleFunc, ruleName, ruleOptions,
                 rulePos, ruleTypeName, seqElems, showClause, showClauseSeq)
import Grammar (isClauseSeqLifted, isNotIgnored)
import Data.Char (isUpper)
import Control.Monad (foldM, foldM_)
import Data.Generics
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Lens

-- explicit import list: mtl 2.2 re-exports all of Control.Monad from this
-- module while mtl >= 2.3 (GHC 9.6's toolchain) does not, so an open import
-- makes the Control.Monad import above unused on one compiler and required
-- on the other - either way fatal under -Werror
import Control.Monad.State.Strict (State, StateT, gets, lift, modify,
                                   runState, runStateT)

-- In the normal form top level clause of the non-lexical rule can be the following:
-- 1. simple_clause *
-- 2. simple_clause +
-- 3. simple_clause ?
-- 4. Seq [simple_clause]
-- 5. alternative of sequences of simple_clause
--
-- simple_clause is one of the following:
-- 1. id
-- 2. Ignore simple_clause
-- 3. Lifted id

data NormalizationState = NormalizationState {
                                              _normSRules :: M.Map ID [SyntaxRule],
                                              _normLRules :: [LexicalRule],
                                              _nameCounter :: Int,
                                              _normAntiRules :: [AntiRule],
                                              _normShortcuts :: [(String, String)],
                                              _proxyRuleNames :: S.Set ID,
                                              _qqLexRuleCache :: M.Map ID ID,
                                              _antiRuleCache :: M.Map ID ID,
                                              _ruleToTypeName :: M.Map ID ID,
                                              -- type name -> rules that receive the $Type:var
                                              -- splice alternative (see computeQQAttachPoints)
                                              _qqAttachPoints :: M.Map ID (S.Set ID),
                                              -- name and position of the rule being normalized,
                                              -- as error context and position fallback
                                              _currentRule :: Maybe (ID, Maybe SourcePos)
                                             }

$(makeLenses ''NormalizationState)

-- Normalization can fail with a structured Diagnostic, carried in the Either
-- under the state transformer.
type Normalization a = StateT NormalizationState (Either Diagnostic) a

setCurrentRule :: GP.Rule -> Normalization ()
setCurrentRule r = currentRule .= Just (ruleName r, rulePos r)

-- Report a grammar error as a Diagnostic at the given position (the
-- offending clause's, when known - the generated AST carries a position on
-- every node), attaching the rule being normalized as context; the rule's
-- header position is the fallback when the node carries none.
normErrorWith :: Maybe SourcePos -> String -> Normalization a
normErrorWith mpos msg = do
  ctx <- gets _currentRule
  let diag = case ctx of
               Just (rn, rpos) -> Diagnostic (maybe rpos Just mpos) (Just ("in rule '" ++ rn ++ "'")) msg
               Nothing         -> Diagnostic mpos Nothing msg
  lift (Left diag)

-- A rule-level error: positioned at the current rule's header.
normError :: String -> Normalization a
normError = normErrorWith Nothing

-- A clause-level error: positioned at the offending clause.
normErrorAt :: GP.Clause -> String -> Normalization a
normErrorAt c = normErrorWith (clausePos c)

newNamePrefixed :: String -> Normalization String
newNamePrefixed prefix = do
  n <- gets _nameCounter
  nameCounter .= (n + 1)
  return $ prefix ++ (show n)

newName :: Normalization String
newName = newNamePrefixed "Rule_"

saveProxyRuleName :: ID -> Normalization ()
saveProxyRuleName ruleName0 = do
  proxyRuleNames %= S.insert ruleName0
  return ()

addRule :: ID -> ID -> SyntaxTopClause -> Normalization ()
addRule tdName ruleName0 clause0 = do
  let doAdd rs = Just $ (SyntaxRule ruleName0 clause0) : (maybe [] id rs)
  normSRules %= M.alter doAdd tdName
  return ()

addShortcut :: String -> String -> Normalization ()
addShortcut strFrom strTo = do
  normShortcuts %= ((strFrom, strTo) :)
  return ()

addAntiRule :: AntiRule -> Normalization ()
addAntiRule rl = do
  normAntiRules %= (rl :)
  return ()

addQQLexRule :: ID -> Normalization ID
addQQLexRule tdName = do
  -- Use deterministic name based on type name, not counter
  let termKindName = "qq_" ++ tdName
      np    = GP.rtkNoPos
      lit s = GP.Lit np (GP.Str np s)
      cl    = foldl1 (GP.Seq np)
                [ lit "$", lit tdName, lit ":"
                , GP.Regex np "a-zA-Z_"
                , GP.Star np (GP.Regex np "A-Za-z0-9_") ]
  addLexicalRule $ LexicalRule "String" "(tail . dropWhile (/= ':'))" termKindName cl
  return termKindName

-- Cached version of addQQLexRule that reuses existing QQ lex rules for the same type
addQQLexRuleCached :: ID -> Normalization ID
addQQLexRuleCached tdName = do
  cache <- gets _qqLexRuleCache
  case M.lookup tdName cache of
    Just lexRuleName -> return lexRuleName  -- Reuse existing rule
    Nothing -> do
      lexRuleName <- addQQLexRule tdName
      qqLexRuleCache %= M.insert tdName lexRuleName  -- Cache it
      return lexRuleName

addLexicalRule :: LexicalRule -> Normalization ()
addLexicalRule lr = do
  normLRules %= (lr :)
  return ()

-- Cached version of anti-rule creation that reuses existing constructors for the same type
-- Only adds the AntiRule to the list ONCE per type, not once per grammar rule
-- Uses deterministic naming: Anti_{TypeName} instead of counter-based names
addAntiRuleCached :: ID -> Bool -> Normalization ID
addAntiRuleCached tdName isList = do
  cache <- gets _antiRuleCache
  case M.lookup tdName cache of
    Just constr -> return constr  -- Reuse existing constructor, don't add duplicate AntiRule
    Nothing -> do
      -- Use deterministic name based on type name, not counter
      let constr = "Anti_" ++ tdName
      addAntiRule $ AntiRule tdName tdName constr isList  -- Only called ONCE per type
      antiRuleCache %= M.insert tdName constr  -- Cache it
      return constr

addRuleWithQQ :: ID -> ID -> SyntaxTopClause -> Normalization ()
addRuleWithQQ tdName ruleName0 clause0 = do
  case clause0 of
    STAltOfSeq altseqs ->
        case L.find (\(STSeq _ ssc) -> case ssc of
                                         (SSLifted _ : _) -> True
                                         _ -> False)
                    altseqs of
          Just _ -> addRule tdName ruleName0 clause0
          Nothing -> qqAdd altseqs
    STMany opType (SSId rule) mcl -> do
                -- For list rules, look up the actual type data name for the element rule
                -- This handles cases where the element rule has a shared type (e.g., Expression : AddExpr)
                typeMap <- use ruleToTypeName
                let elemTypeName = M.findWithDefault rule rule typeMap
                newRule <- addListProxyRule elemTypeName rule ruleName0
                addRule tdName ruleName0 $ STMany opType (SSId newRule) mcl
    _ -> addRule tdName ruleName0 clause0
  where qqAdd altseqs = do
          -- The QQ machinery (lexical rule, anti rule) is created at the first
          -- eligible rule of the type, as before; but the splice alternative
          -- itself is only attached at the group's attach points (a minimal
          -- set of rules from which every rule of a shared-type group is
          -- reachable via unit productions, see computeQQAttachPoints).
          -- Attaching it to every rule instead would add one reduce item per
          -- rule for the same splice token, flooding the parser with
          -- reduce/reduce conflicts.
          qqLexRule <- addQQLexRuleCached tdName     -- Use cached version
          constr <- addAntiRuleCached tdName False   -- Use cached version
          attachMap <- use qqAttachPoints
          let attachHere = S.member ruleName0 $ M.findWithDefault S.empty tdName attachMap
          if attachHere
            then addRule tdName ruleName0 $ STAltOfSeq (STSeq constr [SSId qqLexRule] : altseqs)
            else addRule tdName ruleName0 clause0

addListProxyRule :: ID -> ID -> ID -> Normalization ID
addListProxyRule tdName elemRuleName listName = do
  ruleName0 <- newNamePrefixed $ "ListElem_" ++ listName
  -- The QQ token is named after the LIST rule (e.g. $RuleList:xs for
  -- "RuleList = Rule*"), because GenQ's anti functions for isList rules
  -- splice whole lists: in patterns the anti node binds the entire list,
  -- in expressions it prepends a list variable to the remaining elements.
  -- The anti constructor lives in the ELEMENT's type (Anti_<ElemType>),
  -- since the token parses in element position within the list.
  qqLexRule <- addQQLexRuleCached listName
  constr <- addAntiRuleCached tdName True
  addRule tdName ruleName0 $ STAltOfSeq [STSeq constr [SSId qqLexRule], STSeq "" [SSLifted elemRuleName]]
  return ruleName0

extractClause :: GP.Clause -> Normalization ID
extractClause cl = do
  ruleName0 <- newName
  cl1 <- checkClause cl
  addRule ruleName0 ruleName0 cl1
  saveProxyRuleName ruleName0
  return $ ruleName0

extractSClause :: SyntaxTopClause -> Normalization ID
extractSClause cl = do
  ruleName0 <- newName
  addRule ruleName0 ruleName0 cl
  saveProxyRuleName ruleName0
  return $ ruleName0

processRuleOptions :: GP.Rule -> Normalization ()
processRuleOptions r = do
  let dtName = fromMaybe (ruleName r) (ruleTypeName r)
  mapM_ (\ opt -> case opt of
                    GP.Shortcuts _ ids -> mapM_ (\ shortcut -> do
                                               addShortcut (nameText shortcut) dtName
                                               return ()) ids
                    GP.Symmacro _ -> return ()  -- Handle symmacro option
                    GP.Anti_Option v -> error $ "Normalize: quasi-quotation-only Option"
                                                ++ " cannot come from a grammar file: $" ++ v
                    ) (ruleOptions r)

isSymmacroOption :: GP.Option -> Bool
isSymmacroOption GP.Symmacro{} = True
isSymmacroOption _             = False

--------------------------------------------------------------------------------
-- Clause normalization over the generated AST.
--
-- The functions below dispatch on POSITION, because the generated AST has no
-- canonicalizing wrappers: an Alt/Seq node in rule (or group) position is
-- the rule's own alternation/sequence, while the same node in element
-- position can only have come from a parenthesized group (grammar.pg's
-- 'Clause5 = '(' ,Clause ')'' lifts the group, the parentheses leave no
-- node) and is extracted into a proxy rule. Optional clauses desugar inline
-- - 'c?' becomes an alternation with an empty alternative - since the
-- generated AST cannot represent empty sequences (the old removeOpts
-- pre-pass rewrote them on the retired IClause representation).
--
-- Repetition and option shapes are matched with rtk's own quasi-quoter
-- ([clause| $cl1 * ~ $cl2 |] is the shape in grammar syntax; see Frontend's
-- module notes for the metavariable convention). The constructor spelling
-- stays where it reads better: wildcard tests like GP.Lifted{}, and arms
-- that bind a scalar Name, which the quoter cannot (grammar.pg's Name
-- splices are list-shaped, registered by IdList).
--------------------------------------------------------------------------------

-- A clause in rule (or parenthesized-group) position: an alternation of
-- sequences.
checkClause :: GP.Clause -> Normalization SyntaxTopClause
checkClause c = case altElems c of
  [alt] -> checkSingleAlt alt
  alts  -> STAltOfSeq <$> mapM checkAltSeq alts

-- A sole alternative: a repetition or option here shapes the whole rule
-- (STMany / the desugared option), everything else is a one-alternative
-- alternation.
checkSingleAlt :: GP.Clause -> Normalization SyntaxTopClause
checkSingleAlt (GP.Labeled _ n body) = do
  s <- checkNamedSeq n body
  return $ STAltOfSeq [s]
checkSingleAlt alt = case seqElems alt of
  [el] -> checkSoleElement el
  els  -> do
    s <- checkSeqOf els
    return $ STAltOfSeq [s]

checkSoleElement :: GP.Clause -> Normalization SyntaxTopClause
checkSoleElement c = case c of
  [clause| $cl1 *        |] -> STMany STStar <$> checkRepeated cl1 <*> pure Nothing
  [clause| $cl1 * ~ $cl2 |] -> STMany STStar <$> checkRepeated cl1 <*> (Just <$> checkSimple cl2)
  [clause| $cl1 +        |] -> STMany STPlus <$> checkRepeated cl1 <*> pure Nothing
  [clause| $cl1 + ~ $cl2 |] -> STMany STPlus <$> checkRepeated cl1 <*> (Just <$> checkSimple cl2)
  [clause| $cl1 ?        |] -> desugarOpt cl1
  GP.Lifted{}         -> checkLiftedAlone c
  GP.Ignored{}        -> checkIgnoredAlone c
  GP.Ref _ n          -> return $ STAltOfSeq [STSeq "" [SSId (nameText n)]]
  -- group spines reach here only through extractClause; checkClause
  -- re-flattens them
  GP.Alt{}            -> checkClause c
  GP.Seq{}            -> checkClause c
  GP.Labeled{}        -> checkClause c
  _ -> normErrorAt c $ "this clause cannot be used in a syntax rule: " ++ showClause c
                       ++ " (regular expressions and '.' are only allowed in lexical rules)"

-- c? desugars to ( | c): an empty alternative plus the optional clause.
desugarOpt :: GP.Clause -> Normalization SyntaxTopClause
desugarOpt el = do
  c1 <- checkSimple el
  return $ STAltOfSeq [STSeq "" [], STSeq "" [c1]]

-- A lifted (,) clause names the single rule whose value becomes this rule's
-- value, so it must reference one rule, not a repetition. Lifting a list/plus
-- (e.g. "Foo = ,Bar* ;") is not implemented: it would otherwise slip through to
-- GenAST.genSimpleItem and die there ("lifted rules are not yet implemented").
-- (',Bar?' is fine: the option desugars into a proxy rule and the proxy is
-- lifted.)
checkLiftedAlone :: GP.Clause -> Normalization SyntaxTopClause
checkLiftedAlone c@(GP.Lifted _ body)
  | isStarPlus body =
      normErrorAt c "a lifted (,) clause is not supported under *, + or ?"
  | otherwise = do
      c1 <- checkSimple body
      case c1 of
        SSId idName -> return $ STAltOfSeq [STSeq "" [SSLifted idName]]
        _ -> normErrorAt c $ "lifted (,) cannot be applied to: " ++ showClause body
checkLiftedAlone c = normErrorAt c $ "lifted (,) cannot be applied to: " ++ showClause c

checkIgnoredAlone :: GP.Clause -> Normalization SyntaxTopClause
checkIgnoredAlone c@(GP.Ignored _ body) = do
  c1 <- checkSimple body
  case c1 of
    SSId idName -> return $ STAltOfSeq [STSeq "" [SSIgnore idName]]
    _ -> normErrorAt c $ "ignore (!) cannot be applied to: " ++ showClause body
checkIgnoredAlone c = normErrorAt c $ "ignore (!) cannot be applied to: " ++ showClause c

isStarPlus :: GP.Clause -> Bool
isStarPlus GP.Star{}      = True
isStarPlus GP.StarDelim{} = True
isStarPlus GP.Plus{}      = True
isStarPlus GP.PlusDelim{} = True
isStarPlus _              = False

-- One alternative of a multi-alternative rule.
checkAltSeq :: GP.Clause -> Normalization STSeq
checkAltSeq (GP.Labeled _ n body) = checkNamedSeq n body
checkAltSeq alt = checkSeqOf (seqElems alt)

checkSeqOf :: [GP.Clause] -> Normalization STSeq
checkSeqOf els = do
  cs1 <- mapM checkSimple els
  checkLiftedInSeq els cs1
  return $ STSeq "" cs1

-- A named alternative ("Star: Clause5 '*'"): the body normalizes exactly
-- like the same alternative would unlabeled, and the user's name replaces
-- the generated Ctr__ one. Note this means a label always produces a
-- constructor: a sole "R = L: A* ;" alternative is a data declaration whose
-- L wraps the (proxied) list, where the unlabeled spelling would have made
-- R a bare list alias. Only a lifted (,) alternative cannot be named - it
-- passes another rule's value through and produces no constructor, so a
-- name on it would silently vanish (GenY ignores names on lifted
-- alternatives, GenAST filters them from the data declaration).
checkNamedSeq :: GP.Name -> GP.Clause -> Normalization STSeq
checkNamedSeq n body = do
  STSeq _ cs1 <- checkSeqOf (seqElems body)
  if isClauseSeqLifted cs1
    then normErrorWith (namePos n) $
           "the lifted (,) alternative '" ++ showClauseSeq (seqElems body)
           ++ "' passes another rule's value through and produces no constructor,"
           ++ " so it cannot be named '" ++ nameText n ++ "': remove the label or the ','"
    else return $ STSeq (nameText n) cs1

-- A lifted (,) clause must be the only non-ignored clause of its sequence.
-- Check it here, where the offending clause is still known, instead of failing
-- without context during code generation (see isClauseSeqLifted)
checkLiftedInSeq :: [GP.Clause] -> [SyntaxSimpleClause] -> Normalization ()
checkLiftedInSeq orig cs =
  case filter isNotIgnored cs of
    [SSLifted _] -> return ()
    cs1 | any isLifted cs1 ->
            normErrorWith (firstLiftedPos orig) $
              "a lifted (,) clause cannot be mixed with other clauses in a sequence: "
              ++ showClauseSeq orig
    _ -> return ()
  where isLifted SSLifted{} = True
        isLifted _          = False
        firstLiftedPos os = listToMaybe [ p | o@GP.Lifted{} <- os, Just p <- [clausePos o] ]

checkSimple :: GP.Clause -> Normalization SyntaxSimpleClause
checkSimple (GP.Ref _ n) = return $ SSId (nameText n)
checkSimple (GP.Lifted _ (GP.Ref _ n)) = return $ SSLifted (nameText n)
checkSimple c@(GP.Ignored _ body) = do
  newC1 <- checkSimple body
  case newC1 of
    SSId idName -> return $ SSIgnore idName
    _ -> normErrorAt c $ "ignore (!) cannot be applied to: " ++ showClause body
checkSimple c = extractClause c >>= return . SSId

-- The repeated element of a * or + clause must be a syntax rule: the rule's
-- value is a list of the element's values, and the element's splice support
-- (a ListElem proxy whose Anti_ constructor lives in the element's data
-- type, see addListProxyRule) needs a grammar-owned data declaration to
-- host that constructor. Terminals have neither: an ignored item (a string
-- literal or !-clause) produces no value to collect, and a lexical rule
-- produces a bare token payload with no data declaration. Both used to slip
-- through to the generators and emit uncompilable Haskell (issue #28), as
-- did a lifted (,) element, which crashed GenAST. Reject them here, where
-- the offending clause is still known. The delimiter is unrestricted: it is
-- matched and dropped, so any clause works there.
checkRepeated :: GP.Clause -> Normalization SyntaxSimpleClause
checkRepeated c = do
  c1 <- checkSimple c
  case c1 of
    -- Not rendered via showClause: a string literal is already replaced by
    -- its internal token name (tok_..._N) at this stage, which would leak
    -- into the message; the rule context and position locate the clause.
    SSIgnore _ -> normErrorAt c $ "repetition of an item that produces no value:"
                              ++ " the repeated item is a string literal or !-ignored clause,"
                              ++ " which is matched but dropped;"
                              ++ " wrap the item in a syntax rule and repeat that rule instead"
    SSId name | isLexicalRule name ->
        normErrorAt c $ "repetition over the lexical rule '" ++ name ++ "' is not supported:"
                    ++ " wrap it in a syntax rule (Item = " ++ name ++ " ;) and repeat Item"
                    ++ " to give the list elements a data type"
    SSLifted _ -> normErrorAt c "a lifted (,) clause is not supported under *, + or ?"
    _ -> return c1

normalizeRule :: GP.Rule -> Normalization ()
normalizeRule r | not (isLexicalRule rn) = do
  processRuleOptions r
  newCl <- checkClause (ruleClause r)
  addRuleWithQQ (fromMaybe rn (ruleTypeName r)) rn newCl
  where rn = ruleName r
normalizeRule r = do
  let rn = ruleName r
      (dtn1, df1) = case (ruleTypeName r, ruleFunc r) of
                      (Nothing, Nothing) -> ("String", "id")
                      (Just d,  Nothing) -> (d,        "read")
                      (Just d,   Just f) -> (d,        f)
                      (Nothing,  Just f) -> ("String", f)
  if any isSymmacroOption (ruleOptions r)
    then
      addLexicalRule $ MacroRule rn (ruleClause r)
    else
      addLexicalRule $ LexicalRule dtn1 df1 rn (ruleClause r)

-- Build a map from rule name to type data name for all rules in the grammar.
-- This is needed to look up the correct type when processing list rules.
buildRuleToTypeMap :: [GP.Rule] -> M.Map ID ID
buildRuleToTypeMap rules = M.fromList $ map ruleMapping rules
  where
    ruleMapping r = (ruleName r, fromMaybe (ruleName r) (ruleTypeName r))

-- A rule name may be defined only once: addRule would otherwise silently
-- merge the definitions into one rule group, turning an (almost certainly
-- accidental) duplicate into extra alternatives (issue #20). Checked on the
-- input rules, so the synthesized start wrapper added later by addStartGroup
-- - which legitimately reuses the start rule's name - is exempt.
checkDuplicateRuleNames :: [GP.Rule] -> Normalization ()
checkDuplicateRuleNames = go M.empty
  where
    go _ [] = return ()
    go seen (r : rest) =
      case M.lookup (ruleName r) seen of
        Nothing -> go (M.insert (ruleName r) r seen) rest
        Just firstDef -> do
          setCurrentRule r
          normError $ "rule '" ++ ruleName r ++ "' is defined more than once"
                      ++ firstDefinedAt firstDef
    firstDefinedAt r = case rulePos r of
      Just pos -> " (first definition at " ++ showSourcePos pos ++ ")"
      Nothing  -> ""

-- Explicit constructor names ("Name: ..." alternative labels) are validated
-- up front, while the offending label and its position are still known:
--
--   * the name must be constructor-shaped (start with an uppercase letter;
--     the lexer already restricts it to [A-Za-z0-9_]*);
--   * the generated-name conventions are reserved: 'Ctr__' (Normalize fills
--     unnamed alternatives with such names) and 'Anti_' (the quasi-quotation
--     splice constructors built by addAntiRuleCached);
--   * explicit names must be unique across the WHOLE grammar, not just their
--     own rule: all constructors land in one generated Haskell module, and a
--     duplicate within a shared-type group would additionally be merged
--     silently by GenAST's constructor-name deduplication (which exists for
--     the Anti_ alternatives), dropping one of the user's alternatives;
--   * lexical rules carry no AST constructors, so labels there are rejected
--     rather than silently ignored.
checkCtorLabels :: [GP.Rule] -> Normalization ()
checkCtorLabels = foldM_ checkRule M.empty
  where
    checkRule seen r = do
      setCurrentRule r
      foldM (checkName r) seen (ctorLabels (ruleClause r))
    ctorLabels :: GP.Clause -> [GP.Name]
    ctorLabels = everything (++) ([] `mkQ` label)
      where label (GP.Labeled _ n _) = [n]
            label _                  = []
    checkName r seen nm
      | isLexicalRule (ruleName r) =
          labelError nm $ "a constructor name ('" ++ n ++ ":') cannot appear in a lexical rule:"
                      ++ " tokens carry no AST constructors"
      | not (startsUpper n) =
          labelError nm $ "constructor name '" ++ n ++ "' must start with an uppercase letter"
      | "Ctr__" `L.isPrefixOf` n =
          labelError nm $ "constructor name '" ++ n ++ "' uses the reserved prefix 'Ctr__'"
                      ++ " (rtk generates such names for unnamed alternatives)"
      | "Anti_" `L.isPrefixOf` n =
          labelError nm $ "constructor name '" ++ n ++ "' uses the reserved prefix 'Anti_'"
                      ++ " (rtk generates such names for quasi-quotation splices)"
      | Just firstUse <- M.lookup n seen =
          labelError nm $ "constructor name '" ++ n ++ "' is already used"
                      ++ inRule firstUse
                      ++ ": explicit constructor names must be unique across the grammar"
                      ++ " (all constructors share one generated Haskell module)"
      | otherwise = return $ M.insert n (ruleName r, namePos nm) seen
      where n = nameText nm
    labelError nm = normErrorWith (namePos nm)
    startsUpper (ch : _) = isUpper ch
    startsUpper []       = False
    inRule (rn, mpos) = " in rule '" ++ rn ++ "'"
                        ++ case mpos of
                             Just pos -> " (at " ++ showSourcePos pos ++ ")"
                             Nothing  -> ""

-- Every bare-name reference inside a clause.
allIdRefs :: GP.Clause -> [ID]
allIdRefs = everything (++) ([] `mkQ` idRef)
  where idRef (GP.Ref _ n) = [nameText n]
        idRef _            = []

-- A declared type may exist only through rule annotations ("T : Rule = ..."),
-- with no rule named T - a "pure type group". A reference to such a bare type
-- name would reach the generators as a reference to the nonexistent rule T
-- and die in GenAST.findRuleDataTypeName (issue #14). The reference can be
-- the author's (a plain "S = T ;" or a list element "T* ~ ','") or one the
-- QQ machinery generates itself: the start wrapper added by addStartGroup
-- references every public group's type name, so a data-start grammar fails
-- on a pure type group even when the author never mentions the bare name.
--
-- Synthesize the cover rule that authors write by hand (java.pg's
-- "Expression : Expression = AssignmentExpression ;"), in its lifted form:
--
--     T : T = ,r1 | ,r2 | ... ;     -- one alternative per rule annotated T
--
-- A lifted alternative passes the annotated rule's value through, so the
-- cover adds a nonterminal for T without adding an AST constructor. It is
-- synthesized on the parsed rule list, before any normalization bookkeeping,
-- so it flows through buildRuleToTypeMap and computeQQAttachPoints exactly
-- like a hand-written cover: its alternatives are unit edges r_i -> T, the
-- annotated rules therefore cover the always-demanded T, and a $T:var
-- splice climbs to T through the cover from whatever attach point the
-- greedy cover picks.
--
-- Synthesis is demand-driven: a cover appears only when something will
-- reference T - a bare-name reference in some syntax rule, or the QQ start
-- wrapper. The wrapper exists only for a data start group (an alias start
-- group skips the QQ entry-point machinery, see addStartGroup), so that
-- demand is predicted here with the same group-shape rules normalization
-- applies: the start group ends up an alias exactly when it keeps a single
-- rule whose top-level clause is a repetition - no second user rule of the
-- start type, no synthesized cover joining the group, and no list-element
-- proxy added into it by a repetition over a start-typed element. Grammars
-- that demand no cover are returned untouched, output byte for byte.
synthesizeTypeCovers :: [GP.Rule] -> [GP.Rule]
synthesizeTypeCovers rules
  | null covers = rules
  | otherwise   = rules ++ covers
  where
    synRules  = [ r | r <- rules, not (isLexicalRule (ruleName r)) ]
    ruleNames = S.fromList (map ruleName rules)
    ruleTypeOf r = fromMaybe (ruleName r) (ruleTypeName r)
    typeOfName = M.fromList [ (ruleName r, ruleTypeOf r) | r <- synRules ]

    -- declared types and each type's annotated rules, in declaration order
    declaredTypes = L.nub [ t | r <- synRules, Just t <- [ruleTypeName r] ]
    rulesOfType t = [ ruleName r | r <- synRules, ruleTypeName r == Just t ]
    pureTypes = [ t | t <- declaredTypes, not (t `S.member` ruleNames) ]

    referenced = S.fromList $ concatMap (allIdRefs . ruleClause) synRules
    demanded
      | aliasStart = [ t | t <- pureTypes, t `S.member` referenced ]
      | otherwise  = pureTypes   -- the start wrapper references every type

    covers = map coverRule demanded
    coverRule t = GP.RuleTyped np (ident t) (ident t)
                    (foldl1 (GP.Alt np)
                      [ GP.Lifted np (GP.Ref np (ident r)) | r <- rulesOfType t ])
    np = GP.rtkNoPos
    ident = GP.Ident np

    aliasStart = case synRules of
      []              -> True   -- no wrappers (normalization rejects the grammar first)
      (firstRule : _) ->
        let firstID = ruleTypeOf firstRule
        in case [ r | r <- synRules, ruleTypeOf r == firstID ] of
             -- a cover joins the group when the start type is itself a
             -- referenced pure type
             [r] -> isTopRepetition (ruleClause r)
                    && not (firstID `elem` pureTypes && firstID `S.member` referenced)
                    && all ((/= Just firstID) . topRepetitionElemType) synRules
             _   -> False

    isTopRepetition = isStarPlus

    -- The type of the element a top-level repetition rule collects: such a
    -- rule gets a ListElem proxy added into the ELEMENT type's group (see
    -- addRuleWithQQ/addListProxyRule). A non-reference element is extracted
    -- into a fresh proxy group of its own and can never hit the start group.
    topRepetitionElemType r = case ruleClause r of
        [clause| $cl1 *                  |] -> elemType cl1
        [clause| $cl1 * ~ $Clause:_delim |] -> elemType cl1
        [clause| $cl1 +                  |] -> elemType cl1
        [clause| $cl1 + ~ $Clause:_delim |] -> elemType cl1
        _                                   -> Nothing
      where elemType (GP.Ref _ n) = Just (M.findWithDefault (nameText n) (nameText n) typeOfName)
            elemType _            = Nothing

-- Decide, for every data type, which of its rules receive the $Type:var
-- splice alternative ("attach points"). A type declared by a single rule
-- keeps today's behavior: the rule itself is the attach point.
--
-- For a shared type (several rules declaring the same type, e.g. java.pg's
-- 18-rule Expression precedence chain) attaching the alternative to every
-- rule would make the splice token reducible to all of them at once: the
-- parser gets one reduce item per rule in the same states and cannot know
-- which rule to reduce the token to (in java.pg this used to cause 806 of
-- the 883 reduce/reduce conflicts). Instead the alternative is attached to
-- a minimal set of rules from which the rest of the group is reachable via
-- unit productions:
--
--   * Build the intra-group "lift graph": an edge A -> B when rule B has an
--     alternative consisting of exactly the single nonterminal A once
--     nullable clauses are dropped (i.e. an A on the stack reduces to B by a
--     unit production, possibly surrounded by empty reductions). For java.pg
--     this is the chain PrimaryNoPostfix -> PostfixExpression -> ... ->
--     Expression.
--   * Collect the rules of the group some grammar position actually demands:
--     every reference from any rule's alternatives counts, except the
--     reference that forms a lift edge inside the group itself (a splice
--     never needs to stop at that position: reducing further is the only
--     thing the parser can do with it). The rule named after the type is
--     always demanded, because the synthesized start rule references it.
--   * Greedily pick attach points whose unit-closure covers all demanded
--     rules. A precedence chain needs exactly one (its bottom rule). A group
--     that genuinely needs several attach points gets several; if their
--     closures overlap, the overlap reintroduces reduce/reduce conflicts
--     between the splice reductions -- that is inherent to such a grammar,
--     not to this placement (there is no warning channel in normalization,
--     so such groups are not reported).
--
-- Only rules that addRuleWithQQ would accept the alternative for are attach
-- point candidates (rules whose clause normalizes to plain alternatives with
-- no leading lifted (,) clause). Two conservative fallbacks keep splicing at
-- least as available as before: when no rule is named after the type, every
-- rule of the group counts as demanded; and when the greedy cover cannot
-- cover anything, every candidate becomes an attach point.
computeQQAttachPoints :: [GP.Rule] -> M.Map ID (S.Set ID)
computeQQAttachPoints rules = M.fromList $ map attachFor groups
  where
    synRules = [ r | r <- rules, not (isLexicalRule (ruleName r)) ]
    ruleTypeOf r = fromMaybe (ruleName r) (ruleTypeName r)
    groupNames = L.nub $ map ruleTypeOf synRules
    groups = [ (t, [ r | r <- synRules, ruleTypeOf r == t ]) | t <- groupNames ]
    groupOfRule = M.fromList [ (ruleName r, ruleTypeOf r) | r <- synRules ]

    -- Rule-level nullability, by fixpoint. Lexical rules (and unknown names)
    -- never match the empty input.
    clausesByName = M.fromListWith (flip (++)) [ (ruleName r, [ruleClause r]) | r <- synRules ]
    nullableRules = fixNullable S.empty
    fixNullable env =
      let env' = S.fromList [ n | (n, cls) <- M.toList clausesByName
                                , any (clauseNullable env) cls ]
      in if env' == env then env else fixNullable env'
    clauseNullable env c = case c of
      GP.Ref _ n          -> S.member (nameText n) env
      GP.Lit _ _          -> False
      GP.Dot _            -> False
      GP.Regex _ _        -> False
      GP.Star _ _         -> True
      GP.StarDelim _ _ _  -> True
      GP.Opt _ _          -> True
      GP.Plus _ c1        -> clauseNullable env c1
      GP.PlusDelim _ c1 _ -> clauseNullable env c1
      GP.Alt _ l r        -> clauseNullable env l || clauseNullable env r
      GP.Seq _ l r        -> clauseNullable env l && clauseNullable env r
      GP.Lifted _ c1      -> clauseNullable env c1
      GP.Ignored _ c1     -> clauseNullable env c1
      GP.Labeled _ _ c1   -> clauseNullable env c1
      GP.Anti_Clause _    -> False
    isNullable = clauseNullable nullableRules

    -- The alternatives a clause normalizes to, as element lists (the empty
    -- list is the empty alternative an option desugars to). Mirrors
    -- checkClause/desugarOpt.
    topAlts :: GP.Clause -> [[GP.Clause]]
    topAlts c = case c of
                  GP.Alt{}           -> map seqElems (altElems c)
                  [clause| $cl1 ? |] -> [[], [cl1]]
                  _                  -> [seqElems c]

    -- The non-nullable core of an alternative: Just n for a nonterminal
    -- whose value passes through (a plain or lifted reference), Nothing for
    -- anything opaque (tokens, ignored or repeated material, parenthesized
    -- groups, which normalize to proxy rules).
    coreElems c
      | isNullable c = []
      | otherwise = case c of
          GP.Ref _ n     -> [Just (nameText n)]
          GP.Lifted _ c1 -> coreElems c1
          _              -> [Nothing]
    unitTarget alt = case concatMap coreElems alt of
                       [Just n] -> Just n
                       _        -> Nothing

    -- Mirror of the addRuleWithQQ guard: would this clause get the splice
    -- alternative at all? (STMany rules take the list-proxy path; a leading
    -- lifted (,) clause suppresses the alternative.)
    eligibleClause c = case c of
      GP.Star{}      -> False
      GP.StarDelim{} -> False
      GP.Plus{}      -> False
      GP.PlusDelim{} -> False
      GP.Lifted{}    -> False
      _              -> not $ any altLeadsLifted (topAlts c)
    altLeadsLifted (e : _) = liftedIdHead e
    altLeadsLifted []      = False
    liftedIdHead (GP.Lifted _ (GP.Ref _ _)) = True
    liftedIdHead _                          = False

    attachFor (t, [r]) = (t, S.singleton (ruleName r))
    attachFor (t, rs)  = (t, picked)
      where
        names = L.nub $ map ruleName rs   -- in declaration order
        nameSet = S.fromList names

        -- lift graph: source -> targets it reduces to by a unit production
        edges = M.fromListWith S.union
                  [ (src, S.singleton (ruleName r))
                  | r <- rs
                  , alt <- topAlts (ruleClause r)
                  , Just src <- [unitTarget alt]
                  , src `S.member` nameSet ]
        closureOf n0 = go (S.singleton n0) [n0]
          where go seen [] = seen
                go seen (x:xs) =
                  let new = [ y | y <- S.toList (M.findWithDefault S.empty x edges)
                                , not (S.member y seen) ]
                  in go (foldr S.insert seen new) (new ++ xs)
        closures = M.fromList [ (n, closureOf n) | n <- names ]

        -- rules of this group demanded by some position of the grammar
        demandRefs r alt =
          let refs = filter (`S.member` nameSet) (concatMap allIdRefs alt)
          in case unitTarget alt of
               Just n | M.lookup (ruleName r) groupOfRule == Just t
                      , n `S.member` nameSet -> L.delete n refs
               _ -> refs
        demanded
          | t `S.member` nameSet = S.insert t demanded0
          | otherwise            = nameSet   -- no rule carries the type name:
                                             -- assume everything is demanded
          where demanded0 = S.fromList [ ref | r <- synRules
                                             , alt <- topAlts (ruleClause r)
                                             , ref <- demandRefs r alt ]

        candidates = [ n | n <- names
                         , any (eligibleClause . ruleClause)
                               [ r | r <- rs, ruleName r == n ] ]

        greedy uncovered acc
          | S.null uncovered = reverse acc
          | otherwise = case best of
              Just n  -> greedy (uncovered S.\\ (closures M.! n)) (n : acc)
              Nothing -> reverse acc
          where -- most newly covered demands first; among equals the rule
                -- with the larger closure, i.e. the bottom-most rule of the
                -- chain (a chain bottom and the rule just above it cover the
                -- same demands when the bottom itself is never demanded, but
                -- splices must enter the chain at the bottom to be able to
                -- climb everywhere); declaration order breaks remaining ties
                gain n = (S.size $ (closures M.! n) `S.intersection` uncovered,
                          S.size $ closures M.! n)
                best = fst $ L.foldl' pick (Nothing, (0, 0)) candidates
                pick (b, g0) n = let g = gain n
                                 in if fst g > 0 && g > g0 then (Just n, g) else (b, g0)

        picked = case greedy demanded [] of
                   [] -> S.fromList candidates  -- cover made no progress: keep
                                                -- the old attach-everywhere
                   ns -> S.fromList ns

doNM :: [GP.Rule] -> Normalization ()
doNM rules = do
  checkDuplicateRuleNames rules
  checkCtorLabels rules
  mapM_ (\r -> do setCurrentRule r
                  normalizeRule r)
        rules
  currentRule .= Nothing
  postNormalizeGrammar

postNormalizeGroup :: (ID, [SyntaxRule]) -> Normalization (ID, [SyntaxRule])
postNormalizeGroup g@(_, [_]) = return g
postNormalizeGroup (idName, rules) = do
  newRules <- mapM normRule rules
  return (idName, newRules)
      where
          normRule r@(SyntaxRule _ (STAltOfSeq _)) = return r
          normRule (SyntaxRule rn cl) = do
                                   extractedId <- extractSClause cl
                                   return (SyntaxRule rn (STAltOfSeq [STSeq "" [SSId extractedId]]))

postNormalizeGrammar :: Normalization ()
postNormalizeGrammar = do
  rules <- gets (M.toList . _normSRules)
  newRules <- mapM postNormalizeGroup rules
  normSRules %= flip (foldr $ uncurry M.insert) newRules

-- Whether a group's value is a type alias (a list from a top-level
-- repetition rule, a Maybe from an option rule) rather than a data
-- declaration with constructors. After postNormalizeGrammar such a group
-- has exactly one rule; a group with several rules is all-STAltOfSeq.
isAliasGroup :: SyntaxRuleGroup -> Bool
isAliasGroup = any (not . isAltOfSeq . getSClause) . getSRules
  where isAltOfSeq STAltOfSeq{} = True
        isAltOfSeq _            = False

addStartGroup :: NormalGrammar -> NormalGrammar
-- The QQ entry-point machinery extends the START group's data declaration:
-- one wrapper alternative (dummy tokens around a type reference) per public
-- type, which the per-type quoters parse through and pattern-match on. When
-- the start group is a type alias (the grammar's first rule is a
-- repetition, e.g. "Start = Item* ;"), there is no data declaration to
-- extend: injecting the wrapper rule used to type the start nonterminal
-- both as a data and as a list, generating a parser GHC rejects, plus a
-- dummy-token shift/reduce conflict against the empty list (issue #34).
-- Such a grammar gets no start wrappers, dummy tokens or top-level quoters
-- at all; the element-level splice machinery (qq_* tokens, Anti_*
-- alternatives) is untouched. Note the host/target distinction: a group
-- merely REFERENCED by a wrapper alternative may be an alias, because the
-- alias is just the field type of the start group's wrapper constructor
-- (grammar.pg's "RuleList = Rule*" works this way).
addStartGroup ng@NormalGrammar { getSyntaxRuleGroups = startGroup : _ }
  | isAliasGroup startGroup = ng
addStartGroup ng@NormalGrammar { getSyntaxRuleGroups = rules, getLexicalRules = tokens , getGrammarInfo = info } =
  let proxyRules = getProxyRules info
      (ruleToStartInfo, counter) = foldr
                                     (\el (ruleMap, cnt) ->
                                        let typeName = getSDataTypeName el
                                        in
                                          if S.member typeName proxyRules
                                            then (ruleMap, cnt)
                                            else
                                              (M.insert typeName
                                                        ("tok_" ++ typeName ++ "_dummy_" ++ show cnt)
                                                        ruleMap,
                                                cnt + 1))
                                     (M.empty, getNameCounter info)
                                     rules
      rulesClauses = map (\s ->
                           let typeName = getSDataTypeName s
                               startTok = fromMaybe (error $ "Internal error: no start token generated for type '" ++ typeName ++ "'")
                                                    (M.lookup typeName ruleToStartInfo)
                               dummy = SSIgnore startTok
                           in
                           STSeq "" [dummy,
                                     SSId typeName,
                                     dummy]) $ filterProxyRules proxyRules rules
      newTokens = map (\(_, name) -> LexicalRule { getLRuleDataType = "Keyword",
                                                   getLRuleFunc = "",
                                                   getLRuleName = name,
                                                   getLClause = GP.Lit GP.rtkNoPos (GP.Str GP.rtkNoPos name)}) $ M.toList ruleToStartInfo

      qqRule = SyntaxRule (fromMaybe (error "Internal error: start rule name is not set in grammar info")
                                     (getStartRuleName info))
                          $ STAltOfSeq rulesClauses
    in case rules of
      (startRule:restRules) ->
        -- The wrapper rule shares the start rule's name, and happy merges
        -- same-named rule blocks only when they are adjacent in the .y file
        -- (separated blocks are a hard "Multiple rules for ..." error). The
        -- start group may hold other rules - list-element proxies of lists
        -- over the start type, prepended by addRule after the start rule -
        -- so emit the original start-named rule(s) right after the wrapper
        -- instead of wherever they ended up in the group.
        let startName = getSRuleName qqRule
            (startNamed, others) = L.partition ((== startName) . getSRuleName)
                                               (getSRules startRule)
        in
        ng { getSyntaxRuleGroups = startRule { getSRules = qqRule : startNamed ++ others }: restRules,
             getLexicalRules = newTokens ++ tokens,
             getGrammarInfo = info { getNameCounter = counter, getRuleToStartInfo = ruleToStartInfo }}
      [] -> error "Grammar must have at least one rule group"

normalizeTopLevelClauses :: GP.Grammar -> Either Diagnostic NormalGrammar
normalizeTopLevelClauses g0 =
  let gname = grammarName g0
      rules = synthesizeTypeCovers (grammarRules g0) in
  case rules of
    [] -> Left $ Diagnostic Nothing Nothing $
                   "grammar '" ++ gname ++ "' contains no rules"
    (firstRule:_) -> do
      let firstID = fromMaybe (ruleName firstRule) (ruleTypeName firstRule)
          ruleTypeMap = buildRuleToTypeMap rules
          attachPoints = computeQQAttachPoints rules
      (_, NormalizationState nrs nls counter antiRules shortcuts proxyRules _ _ _ _ _) <-
        runStateT (doNM rules) (NormalizationState M.empty [] 0 [] [] S.empty M.empty M.empty ruleTypeMap attachPoints Nothing)
      firstRuleGroupRules <- case M.lookup firstID nrs of
        Just rs -> Right rs
        Nothing -> Left $ Diagnostic (rulePos firstRule) Nothing $
                            "the first rule ('" ++ ruleName firstRule
                            ++ "') must be a syntax rule (its name must start with an uppercase letter),"
                            ++ " because it defines the start symbol of the grammar"
      let nrs1 = M.delete firstID nrs
          firstGroup = SyntaxRuleGroup firstID firstRuleGroupRules
          otherGroups = map (\ (k,v) -> SyntaxRuleGroup k v) $ M.toList nrs1
          groups = firstGroup : otherGroups
      return $ addStartGroup $ NormalGrammar gname groups nls antiRules shortcuts (grammarImports g0) (GrammarInfo (Just firstID) M.empty counter proxyRules)

data FillNameState = FillNameState { nameCtr :: Int, nameBase :: String }
type FillName a = State FillNameState a

newConstructorName :: FillName String
newConstructorName = do
    n <- gets nameCtr
    b <- gets nameBase
    modify $ (\ s -> s{nameCtr = n + 1})
    return $ "Ctr__" ++ b ++ "__" ++  (show n)

fillConstructorName :: String -> STSeq -> FillName STSeq
fillConstructorName _ (STSeq "" l) = do
    n <- newConstructorName
    return $ STSeq n l
fillConstructorName _ seqValue = return seqValue

fillConstructorNames :: NormalGrammar -> NormalGrammar
fillConstructorNames ng@NormalGrammar { getSyntaxRuleGroups = rules, getGrammarInfo = info } =
    ng { getSyntaxRuleGroups = newrules, getGrammarInfo = info }
      where newrules = map (\r -> doRename (getSDataTypeName r) r) rules
            doRename n dat = let (dat1, (FillNameState _ _)) = runState (everywhereM (mkM (fillConstructorName n)) dat) (FillNameState 0 n)
                               in dat1
