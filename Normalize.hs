{-# LANGUAGE TemplateHaskell #-}
module Normalize(normalizeTopLevelClauses, fillConstructorNames)
    where

import Parser
import Data.Generics
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Control.Lens

import Control.Monad.State.Strict hiding (lift)

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
                                              _antiRuleCache :: M.Map ID ID
                                             }

$(makeLenses ''NormalizationState)

type Normalization a = State NormalizationState a

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
                -- TODO: td name for the rule
                newRule <- addListProxyRule rule rule ruleName
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
  qqLexRule <- addQQLexRuleCached listName    -- Use cached version
  constr <- addAntiRuleCached tdName True     -- Use cached version
  -- For shared types, add anti-alternative to ALL rules (GenAST deduplicates constructors)
  -- This ensures splicing works in all grammar contexts, not just the first rule
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
    _ -> error $ "Ignore cannot be applied to " ++ show c1
checkSimpleClause c = extractClause c >>= return . SSId

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
checkNormalClause (ILifted c) = do
  c1 <- checkSimpleClause c
  case c1 of
    SSId idName -> return $ STAltOfSeq [STSeq "" [SSLifted idName]]
    _ -> error $ "Lifted cannot be applied to " ++ show c1
checkNormalClause (IIgnore c) = do
  c1 <- checkSimpleClause c
  case c1 of
    SSId idName -> return $ STAltOfSeq [STSeq "" [SSIgnore idName]]
    _ -> error $ "Ignore cannot be applied to " ++ show c1
checkNormalClause (IId idName) = do
  return $ STAltOfSeq [STSeq "" [SSId idName]]
checkNormalClause c = error $ "Wrong clause " ++ show c

checkNormalClauseSeq :: IClause -> Normalization STSeq
checkNormalClauseSeq (ISeq cs) = do
  cs1 <- mapM checkSimpleClause cs
  return $ STSeq "" cs1
checkNormalClauseSeq ic = do
  c1 <- checkSimpleClause ic
  return $ STSeq "" [c1]

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

doNM :: InitialGrammar -> Normalization ()
doNM grammar = do
  let grammar0 = everywhereBut (False `mkQ` (isLexicalRule . getIRuleName)) (mkT removeOpts) grammar
  mapM_ normalizeRule $ getIRules grammar0
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
                               dummy = SSIgnore (fromJust (M.lookup typeName ruleToStartInfo))
                           in 
                           STSeq "" [dummy,
                                     SSId typeName,
                                     dummy]) $ filterProxyRules proxyRules rules
      newTokens = map (\(_, name) -> LexicalRule { getLRuleDataType = "Keyword",
                                                   getLRuleFunc = "",
                                                   getLRuleName = name, getLClause = (IStrLit name)}) $ M.toList ruleToStartInfo
      
      qqRule = SyntaxRule (fromJust (getStartRuleName info)) $ STAltOfSeq rulesClauses
    in case rules of
      (startRule:restRules) ->
        ng { getSyntaxRuleGroups = startRule { getSRules = qqRule : getSRules startRule }: restRules,
             getLexicalRules = newTokens ++ tokens,
             getGrammarInfo = info { getNameCounter = counter, getRuleToStartInfo = ruleToStartInfo }}
      [] -> error "Grammar must have at least one rule group"

normalizeTopLevelClauses :: InitialGrammar -> NormalGrammar
normalizeTopLevelClauses grammar =
  case getIRules grammar of
    [] -> error $ "Grammar '" ++ (getIGrammarName grammar) ++ "' contains no rules"
    (firstIRule:_) ->
      let firstID = getIRuleName firstIRule
          (_, NormalizationState nrs nls counter antiRules shortcuts proxyRules _ _) =
            runState (doNM grammar) (NormalizationState M.empty [] 0 [] [] S.empty M.empty M.empty)
          firstRuleGroupRules = fromJust $ M.lookup firstID nrs
          nrs1 = M.delete firstID nrs
          firstGroup = SyntaxRuleGroup firstID firstRuleGroupRules
          otherGroups = map (\ (k,v) -> SyntaxRuleGroup k v) $ M.toList nrs1
          groups = firstGroup : otherGroups
        in addStartGroup $ NormalGrammar (getIGrammarName grammar) groups nls antiRules shortcuts (getImports grammar) (GrammarInfo (Just firstID) M.empty counter proxyRules)

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
