{-# LANGUAGE TemplateHaskell #-}
module Normalize(normalizeTopLevelClauses, fillConstructorNames)
    where

import Syntax
import Diagnostics (Diagnostic(..), showSourcePos)
import Grammar (isNotIgnored)
import Data.Generics
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Lens

import Control.Monad.State.Strict hiding (lift)
import Control.Monad.State.Strict (lift)

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
                                              _currentRule :: Maybe IRule
                                             }

$(makeLenses ''NormalizationState)

-- Normalization can fail with a structured Diagnostic, carried in the Either
-- under the state transformer.
type Normalization a = StateT NormalizationState (Either Diagnostic) a

-- Report a grammar error as a Diagnostic, attaching the rule being normalized
-- (its name as context and its source position, when known) when the problem
-- was found.
normError :: String -> Normalization a
normError msg = do
  ctx <- gets _currentRule
  let diag = case ctx of
               Just r  -> Diagnostic (getIRulePos r) (Just ("in rule '" ++ getIRuleName r ++ "'")) msg
               Nothing -> Diagnostic Nothing Nothing msg
  lift (Left diag)

newNamePrefixed :: String -> Normalization String
newNamePrefixed prefix = do
  n <- gets _nameCounter
  nameCounter .= (n + 1)
  return $ prefix ++ (show n)

newName :: Normalization String
newName = newNamePrefixed "Rule_"

saveProxyRuleName :: ID -> Normalization ()
saveProxyRuleName ruleName = do
  proxyRuleNames %= S.insert ruleName
  return ()

addRule :: ID -> ID -> SyntaxTopClause -> Normalization ()
addRule tdName ruleName clause = do
  let doAdd rs = Just $ (SyntaxRule ruleName clause) : (maybe [] id rs)
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
  addLexicalRule $ LexicalRule "String" "(tail . dropWhile (/= ':'))" termKindName
                     (IAlt [ISeq [IStrLit "$",
                                  IStrLit tdName,
                                  IStrLit ":",
                                  IRegExpLit "a-zA-Z_",
                                  IStar (IRegExpLit "A-Za-z0-9_") Nothing]])
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
addRuleWithQQ tdName ruleName clause = do
  case clause of
    STAltOfSeq altseqs ->
        case L.find (\(STSeq _ ssc) -> case ssc of
                                         (SSLifted _ : _) -> True
                                         _ -> False)
                    altseqs of
          Just _ -> addRule tdName ruleName clause
          Nothing -> qqAdd altseqs
    STMany opType (SSId rule) mcl -> do
                -- For list rules, look up the actual type data name for the element rule
                -- This handles cases where the element rule has a shared type (e.g., Expression : AddExpr)
                typeMap <- use ruleToTypeName
                let elemTypeName = M.findWithDefault rule rule typeMap
                newRule <- addListProxyRule elemTypeName rule ruleName
                addRule tdName ruleName $ STMany opType (SSId newRule) mcl
    _ -> addRule tdName ruleName clause
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
          let attachHere = S.member ruleName $ M.findWithDefault S.empty tdName attachMap
          if attachHere
            then addRule tdName ruleName $ STAltOfSeq (STSeq constr [SSId qqLexRule] : altseqs)
            else addRule tdName ruleName clause

addListProxyRule :: ID -> ID -> ID -> Normalization ID
addListProxyRule tdName elemRuleName listName = do
  ruleName <- newNamePrefixed $ "ListElem_" ++ listName
  -- The QQ token is named after the LIST rule (e.g. $RuleList:xs for
  -- "RuleList = Rule*"), because GenQ's anti functions for isList rules
  -- splice whole lists: in patterns the anti node binds the entire list,
  -- in expressions it prepends a list variable to the remaining elements.
  -- The anti constructor lives in the ELEMENT's type (Anti_<ElemType>),
  -- since the token parses in element position within the list.
  qqLexRule <- addQQLexRuleCached listName
  constr <- addAntiRuleCached tdName True
  addRule tdName ruleName $ STAltOfSeq [STSeq constr [SSId qqLexRule], STSeq "" [SSLifted elemRuleName]]
  return ruleName

extractClause :: IClause -> Normalization ID
extractClause cl = do
  ruleName <- newName
  cl1 <- checkNormalClause cl
  addRule ruleName ruleName cl1
  saveProxyRuleName ruleName
  return $ ruleName

extractSClause :: SyntaxTopClause -> Normalization ID
extractSClause cl = do
  ruleName <- newName
  addRule ruleName ruleName cl
  saveProxyRuleName ruleName
  return $ ruleName

processRuleOptions :: IRule -> Normalization ()
processRuleOptions IRule{getIDataTypeName=dtn, getIRuleName=rn, getIRuleOptions=ropts} = do
  let dtName = (maybe rn Prelude.id dtn)
  mapM_ (\ opt -> case opt of
                    OShortcuts lst -> mapM_ (\ shortcut -> do
                                               addShortcut shortcut dtName
                                               return ()) lst
                    OSymmacro -> return ()  -- Handle symmacro option
                    ) ropts

checkSimpleClause :: IClause -> Normalization SyntaxSimpleClause
checkSimpleClause (IId idName) = return $ SSId idName
checkSimpleClause (ILifted (IId idName)) = return $ SSLifted idName
checkSimpleClause (IIgnore c1) = do
  newC1 <- checkSimpleClause c1
  case newC1 of
    SSId idName -> return $ SSIgnore idName
    _ -> normError $ "ignore (!) cannot be applied to: " ++ showClause c1
checkSimpleClause c = extractClause c >>= return . SSId

-- A repetition/option clause: cannot be the body of a lifted (,) clause.
isRepetition :: IClause -> Bool
isRepetition IStar{} = True
isRepetition IPlus{} = True
isRepetition IOpt{}  = True
isRepetition _       = False

-- The repeated element of a * or + clause must be a syntax rule: the rule's
-- value is a list of the element's values, and the element's splice support
-- (a ListElem proxy whose Anti_ constructor lives in the element's data
-- type, see addListProxyRule) needs a grammar-owned data declaration to
-- host that constructor. Terminals have neither: an ignored item (a string
-- literal or !-clause) produces no value to collect, and a lexical rule
-- produces a bare token payload with no data declaration. Both used to slip
-- through to the generators and emit uncompilable Haskell (issue #28), as
-- did a lifted (,) element, which crashed GenAST. Reject them here, where
-- the offending rule is still known. The delimiter is unrestricted: it is
-- matched and dropped, so any clause works there.
checkRepeatedClause :: IClause -> Normalization SyntaxSimpleClause
checkRepeatedClause c = do
  c1 <- checkSimpleClause c
  case c1 of
    -- Not rendered via showClause: a string literal is already replaced by
    -- its internal token name (tok_..._N) at this stage, which would leak
    -- into the message; the rule context and position locate the clause.
    SSIgnore _ -> normError $ "repetition of an item that produces no value:"
                              ++ " the repeated item is a string literal or !-ignored clause,"
                              ++ " which is matched but dropped;"
                              ++ " wrap the item in a syntax rule and repeat that rule instead"
    SSId name | isLexicalRule name ->
        normError $ "repetition over the lexical rule '" ++ name ++ "' is not supported:"
                    ++ " wrap it in a syntax rule (Item = " ++ name ++ " ;) and repeat Item"
                    ++ " to give the list elements a data type"
    SSLifted _ -> normError "a lifted (,) clause is not supported under *, + or ?"
    _ -> return c1

checkNormalClause :: IClause -> Normalization SyntaxTopClause
checkNormalClause (IStar c mc) = do
  c1 <- checkRepeatedClause c
  c2l <- mapM checkSimpleClause (maybeToList mc)
  return $ STMany STStar c1 (listToMaybe c2l)
checkNormalClause (IPlus c mc) = do
  c1 <- checkRepeatedClause c
  c2l <- mapM checkSimpleClause (maybeToList mc)
  return $ STMany STPlus c1 (listToMaybe c2l)
checkNormalClause (IOpt c) = do
  c1 <- checkSimpleClause c
  return $ STOpt c1
checkNormalClause (IAlt [c]) = do
  checkNormalClause c
checkNormalClause (IAlt cs) = do
  cs1 <- mapM checkNormalClauseSeq cs
  return $ STAltOfSeq cs1
checkNormalClause (ISeq [c]) = do
  checkNormalClause c
checkNormalClause tc@(ISeq _) = do
  c1 <- checkNormalClauseSeq tc
  return $ STAltOfSeq [c1]
-- A lifted (,) clause names the single rule whose value becomes this rule's
-- value, so it must reference one rule, not a repetition. Lifting a list/plus
-- (e.g. "Foo = ,Bar* ;") is not implemented: it would otherwise slip through to
-- GenAST.genSimpleItem and die there ("lifted rules are not yet implemented").
-- (IOpt is desugared by removeOpts before normalization, so only * and + reach
-- here as repetitions.)
checkNormalClause (ILifted c)
  | isRepetition c =
      normError "a lifted (,) clause is not supported under *, + or ?"
  | otherwise = do
      c1 <- checkSimpleClause c
      case c1 of
        SSId idName -> return $ STAltOfSeq [STSeq "" [SSLifted idName]]
        _ -> normError $ "lifted (,) cannot be applied to: " ++ showClause c
checkNormalClause (IIgnore c) = do
  c1 <- checkSimpleClause c
  case c1 of
    SSId idName -> return $ STAltOfSeq [STSeq "" [SSIgnore idName]]
    _ -> normError $ "ignore (!) cannot be applied to: " ++ showClause c
checkNormalClause (IId idName) = do
  return $ STAltOfSeq [STSeq "" [SSId idName]]
checkNormalClause c = normError $ "this clause cannot be used in a syntax rule: " ++ showClause c
                                  ++ " (regular expressions and '.' are only allowed in lexical rules)"

checkNormalClauseSeq :: IClause -> Normalization STSeq
checkNormalClauseSeq (ISeq cs) = do
  cs1 <- mapM checkSimpleClause cs
  checkLiftedInSeq cs cs1
  return $ STSeq "" cs1
checkNormalClauseSeq ic = do
  c1 <- checkSimpleClause ic
  return $ STSeq "" [c1]

-- A lifted (,) clause must be the only non-ignored clause of its sequence.
-- Check it here, where the offending rule is still known, instead of failing
-- without context during code generation (see isClauseSeqLifted)
checkLiftedInSeq :: [IClause] -> [SyntaxSimpleClause] -> Normalization ()
checkLiftedInSeq orig cs =
  case filter isNotIgnored cs of
    [SSLifted _] -> return ()
    cs1 | any isLifted cs1 -> normError $ "a lifted (,) clause cannot be mixed with other clauses in a sequence: "
                                          ++ showClause (ISeq orig)
    _ -> return ()
  where isLifted SSLifted{} = True
        isLifted _          = False

normalizeRule :: IRule -> Normalization ()
normalizeRule r@IRule{getIDataTypeName=dtn, getIRuleName=rn, getIClause=cl, getIDataFunc=_, getIRuleOptions=_} | not (isLexicalRule rn) = do
  processRuleOptions r
  newCl <- checkNormalClause cl
  addRuleWithQQ (maybe rn Prelude.id dtn) rn newCl
normalizeRule r@IRule{getIDataTypeName=dtn, getIDataFunc=df, getIRuleName=rn, getIClause=cl, getIRuleOptions=_} | (isLexicalRule rn) = do
  let (dtn1, df1) = case (dtn, df) of
                      (Nothing, Nothing) -> ("String", "id")
                      (Just d,  Nothing) -> (d,        "read")
                      (Just d,   Just f) -> (d,        f)
                      (Nothing,  Just f) -> ("String", f)
  if (OSymmacro `elem` (getIRuleOptions r))
    then
      addLexicalRule $ MacroRule rn cl
    else
      addLexicalRule $ LexicalRule dtn1 df1 rn cl
normalizeRule r = error $ "normalizeRule: unexpected rule pattern: " ++ show r

-- Build a map from rule name to type data name for all rules in the grammar.
-- This is needed to look up the correct type when processing list rules.
buildRuleToTypeMap :: InitialGrammar -> M.Map ID ID
buildRuleToTypeMap grammar = M.fromList $ map ruleMapping $ getIRules grammar
  where
    ruleMapping r = (getIRuleName r, maybe (getIRuleName r) id (getIDataTypeName r))

-- A rule name may be defined only once: addRule would otherwise silently
-- merge the definitions into one rule group, turning an (almost certainly
-- accidental) duplicate into extra alternatives (issue #20). Checked on the
-- input rules, so the synthesized start wrapper added later by addStartGroup
-- - which legitimately reuses the start rule's name - is exempt.
checkDuplicateRuleNames :: [IRule] -> Normalization ()
checkDuplicateRuleNames = go M.empty
  where
    go _ [] = return ()
    go seen (r : rest) =
      case M.lookup (getIRuleName r) seen of
        Nothing -> go (M.insert (getIRuleName r) r seen) rest
        Just firstDef -> do
          currentRule .= Just r
          normError $ "rule '" ++ getIRuleName r ++ "' is defined more than once"
                      ++ firstDefinedAt firstDef
    firstDefinedAt r = case getIRulePos r of
      Just pos -> " (first definition at " ++ showSourcePos pos ++ ")"
      Nothing  -> ""

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
computeQQAttachPoints :: InitialGrammar -> M.Map ID (S.Set ID)
computeQQAttachPoints grammar = M.fromList $ map attachFor groups
  where
    synRules = [ r | r <- getIRules grammar, not (isLexicalRule (getIRuleName r)) ]
    ruleTypeOf r = maybe (getIRuleName r) Prelude.id (getIDataTypeName r)
    groupNames = L.nub $ map ruleTypeOf synRules
    groups = [ (t, [ r | r <- synRules, ruleTypeOf r == t ]) | t <- groupNames ]
    groupOfRule = M.fromList [ (getIRuleName r, ruleTypeOf r) | r <- synRules ]

    -- Rule-level nullability, by fixpoint. Lexical rules (and unknown names)
    -- never match the empty input.
    clausesByName = M.fromListWith (flip (++)) [ (getIRuleName r, [getIClause r]) | r <- synRules ]
    nullableRules = fixNullable S.empty
    fixNullable env =
      let env' = S.fromList [ n | (n, cls) <- M.toList clausesByName
                                , any (clauseNullable env) cls ]
      in if env' == env then env else fixNullable env'
    clauseNullable env c = case c of
      IId n        -> S.member n env
      IStrLit _    -> False
      IDot         -> False
      IRegExpLit _ -> False
      IStar _ _    -> True
      IOpt _       -> True
      IPlus c1 _   -> clauseNullable env c1
      IAlt cs      -> any (clauseNullable env) cs
      ISeq cs      -> all (clauseNullable env) cs
      ILifted c1   -> clauseNullable env c1
      IIgnore c1   -> clauseNullable env c1
    isNullable = clauseNullable nullableRules

    -- The alternatives a clause normalizes to, mirroring checkNormalClause:
    -- singleton wrappers unwrap, a top-level option contributes an empty
    -- alternative (removeOpts), everything else is a single alternative.
    unwrapTop (IAlt [c]) = unwrapTop c
    unwrapTop (ISeq [c]) = unwrapTop c
    unwrapTop c          = c
    topAlts c = case unwrapTop c of
                  IAlt cs -> cs
                  IOpt c1 -> [ISeq [], ISeq [c1]]
                  c'      -> [c']

    -- The non-nullable core of an alternative: Just n for a nonterminal
    -- whose value passes through (a plain or lifted reference), Nothing for
    -- anything opaque (tokens, ignored or repeated material, nested
    -- alternatives, which normalize to proxy rules).
    coreElems c
      | isNullable c = []
      | otherwise = case c of
          ISeq cs    -> concatMap coreElems cs
          IId n      -> [Just n]
          ILifted c1 -> coreElems c1
          _          -> [Nothing]
    unitTarget alt = case coreElems alt of
                       [Just n] -> Just n
                       _        -> Nothing

    -- Mirror of the addRuleWithQQ guard: would this clause get the splice
    -- alternative at all? (STMany rules take the list-proxy path; a leading
    -- lifted (,) clause suppresses the alternative.)
    eligibleClause c = case unwrapTop c of
      IStar{}   -> False
      IPlus{}   -> False
      ILifted _ -> False
      c'        -> not $ any altLeadsLifted (topAlts c')
    altLeadsLifted (ISeq (c:_)) = liftedIdHead c
    altLeadsLifted c            = liftedIdHead c
    liftedIdHead (ILifted (IId _)) = True
    liftedIdHead _                 = False

    allIdRefs :: IClause -> [ID]
    allIdRefs = everything (++) ([] `mkQ` idRef)
      where idRef (IId n) = [n]
            idRef _       = []

    attachFor (t, [r]) = (t, S.singleton (getIRuleName r))
    attachFor (t, rs)  = (t, picked)
      where
        names = L.nub $ map getIRuleName rs   -- in declaration order
        nameSet = S.fromList names

        -- lift graph: source -> targets it reduces to by a unit production
        edges = M.fromListWith S.union
                  [ (src, S.singleton (getIRuleName r))
                  | r <- rs
                  , alt <- topAlts (getIClause r)
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
          let refs = filter (`S.member` nameSet) (allIdRefs alt)
          in case unitTarget alt of
               Just n | M.lookup (getIRuleName r) groupOfRule == Just t
                      , n `S.member` nameSet -> L.delete n refs
               _ -> refs
        demanded
          | t `S.member` nameSet = S.insert t demanded0
          | otherwise            = nameSet   -- no rule carries the type name:
                                             -- assume everything is demanded
          where demanded0 = S.fromList [ ref | r <- synRules
                                             , alt <- topAlts (getIClause r)
                                             , ref <- demandRefs r alt ]

        candidates = [ n | n <- names
                         , any (eligibleClause . getIClause)
                               [ r | r <- rs, getIRuleName r == n ] ]

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

doNM :: InitialGrammar -> Normalization ()
doNM grammar = do
  let grammar0 = everywhereBut (False `mkQ` (isLexicalRule . getIRuleName)) (mkT removeOpts) grammar
  checkDuplicateRuleNames $ getIRules grammar0
  mapM_ (\r -> do currentRule .= Just r
                  normalizeRule r)
        $ getIRules grammar0
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
                                                   getLRuleName = name, getLClause = (IStrLit name)}) $ M.toList ruleToStartInfo
      
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

normalizeTopLevelClauses :: InitialGrammar -> Either Diagnostic NormalGrammar
normalizeTopLevelClauses grammar =
  case getIRules grammar of
    [] -> Left $ Diagnostic Nothing Nothing $
                   "grammar '" ++ getIGrammarName grammar ++ "' contains no rules"
    (firstIRule:_) -> do
      let firstID = maybe (getIRuleName firstIRule) Prelude.id (getIDataTypeName firstIRule)
          ruleTypeMap = buildRuleToTypeMap grammar
          attachPoints = computeQQAttachPoints grammar
      (_, NormalizationState nrs nls counter antiRules shortcuts proxyRules _ _ _ _ _) <-
        runStateT (doNM grammar) (NormalizationState M.empty [] 0 [] [] S.empty M.empty M.empty ruleTypeMap attachPoints Nothing)
      firstRuleGroupRules <- case M.lookup firstID nrs of
        Just rs -> Right rs
        Nothing -> Left $ Diagnostic (getIRulePos firstIRule) Nothing $
                            "the first rule ('" ++ getIRuleName firstIRule
                            ++ "') must be a syntax rule (its name must start with an uppercase letter),"
                            ++ " because it defines the start symbol of the grammar"
      let nrs1 = M.delete firstID nrs
          firstGroup = SyntaxRuleGroup firstID firstRuleGroupRules
          otherGroups = map (\ (k,v) -> SyntaxRuleGroup k v) $ M.toList nrs1
          groups = firstGroup : otherGroups
      return $ addStartGroup $ NormalGrammar (getIGrammarName grammar) groups nls antiRules shortcuts (getImports grammar) (GrammarInfo (Just firstID) M.empty counter proxyRules)

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

removeOpts :: IClause -> IClause
removeOpts (IOpt c) = IAlt [ISeq [], ISeq [c]]
removeOpts a = a
