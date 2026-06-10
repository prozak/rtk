{-# LANGUAGE TemplateHaskell #-}
module Normalize(normalizeTopLevelClauses, fillConstructorNames)
    where

import Parser
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
          qqLexRule <- addQQLexRuleCached tdName     -- Use cached version
          constr <- addAntiRuleCached tdName False   -- Use cached version
          -- For shared types, add anti-alternative to ALL rules (GenAST deduplicates constructors)
          -- This ensures splicing works in all grammar contexts, not just the first rule
          addRule tdName ruleName $ STAltOfSeq (STSeq constr [SSId qqLexRule] : altseqs)

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

checkNormalClause :: IClause -> Normalization SyntaxTopClause
checkNormalClause (IStar c mc) = do
  c1 <- checkSimpleClause c
  c2l <- mapM checkSimpleClause (maybeToList mc)
  return $ STMany STStar c1 (listToMaybe c2l)
checkNormalClause (IPlus c mc) = do
  c1 <- checkSimpleClause c
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

addStartGroup :: NormalGrammar -> NormalGrammar
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
        ng { getSyntaxRuleGroups = startRule { getSRules = qqRule : getSRules startRule }: restRules,
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
      (_, NormalizationState nrs nls counter antiRules shortcuts proxyRules _ _ _ _) <-
        runStateT (doNM grammar) (NormalizationState M.empty [] 0 [] [] S.empty M.empty M.empty ruleTypeMap Nothing)
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
