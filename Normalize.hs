module Normalize(normalizeTopLevelClauses, fillConstructorNames)
    where

import Parser
import Grammar
import Data.Char
import Data.Generics
import Data.Data
import Data.Maybe
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S

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
                                              normSRules :: M.Map ID [SyntaxRule],
                                              normLRules :: [LexicalRule],
                                              nameCounter :: Int,
                                              normAntiRules :: [AntiRule],
                                              normShortcuts :: [(String, String)],
                                              proxyRuleNames :: S.Set ID 
                                             }
type Normalization a = State NormalizationState a

newNamePrefixed :: String -> Normalization String
newNamePrefixed prefix = do
  n <- gets nameCounter
  modify $ (\ s -> s{nameCounter = n + 1})
  return $ prefix ++ (show n)

newName :: Normalization String
newName = newNamePrefixed "Rule_"

saveProxyRuleName :: ID -> Normalization ()
saveProxyRuleName ruleName = do
  modify (\s@NormalizationState { proxyRuleNames = srl } -> s { proxyRuleNames = S.insert ruleName srl })
  return ()

addRule :: ID -> ID -> SyntaxTopClause -> Normalization ()
addRule tdName ruleName clause = do
  let doAdd rs = Just $ (SyntaxRule ruleName clause):(maybe [] id rs)
  modify (\ s@NormalizationState{ normSRules = nr } -> s{ normSRules = M.alter doAdd tdName nr } )
  return ()

addShortcut :: String -> String -> Normalization ()
addShortcut strFrom strTo = do
  modify (\ s@NormalizationState{ normShortcuts = ns } -> s{ normShortcuts = (strFrom, strTo):ns } )
  return ()

addAntiRule :: AntiRule -> Normalization ()
addAntiRule rl = do
  modify (\ s@NormalizationState{ normAntiRules = na } -> s{ normAntiRules = rl : na } )
  return ()

addQQLexRule :: ID -> Normalization ID
addQQLexRule tdName = do
  termKindName <- newNamePrefixed $ "qq_" ++ tdName
  addLexicalRule $ LexicalRule "String" "(tail . dropWhile (/= ':'))" termKindName 
                     (IAlt [ISeq [IStrLit "$",
                                  IStrLit tdName,
                                  IStrLit ":",
                                  IRegExpLit "a-zA-Z_",
                                  IStar (IRegExpLit "A-Za-z0-9_") Nothing]])
  return termKindName

addLexicalRule :: LexicalRule -> Normalization ()
addLexicalRule lr = do
  modify $ \s@NormalizationState{ normLRules = nr} -> s{ normLRules = lr:nr }
  return ()

addRuleWithQQ :: ID -> ID -> SyntaxTopClause -> Normalization ()
addRuleWithQQ tdName ruleName clause = do
  case clause of
    STAltOfSeq altseqs ->
        case L.find (\(STSeq _ ssc) -> case ssc of
                                         (SSLifted _ : others) -> True
                                         _ -> False)
                    altseqs of
          Just _ -> addRule tdName ruleName clause
          Nothing -> qqAdd altseqs
    STMany op (SSId rule) mcl -> do
                -- TODO: td name for the rule
                newRule <- addListProxyRule rule rule ruleName
                addRule tdName ruleName $ STMany op (SSId newRule) mcl
    _ -> addRule tdName ruleName clause
  where qqAdd altseqs = do
          qqLexRule <- addQQLexRule tdName
          constr <- newNamePrefixed $ "Anti_" ++ tdName
          addAntiRule $ AntiRule tdName tdName constr False
          addRule tdName ruleName $ STAltOfSeq (STSeq constr [SSId qqLexRule] : altseqs)

addListProxyRule :: ID -> ID -> ID -> Normalization ID
addListProxyRule tdName elemRuleName listName = do
  ruleName <- newNamePrefixed $ "ListElem_" ++ listName
  qqLexRule <- addQQLexRule listName
  constr <- newNamePrefixed $ "Anti_" ++ listName
  addAntiRule $ AntiRule tdName listName constr True
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

{-
isSimpleClause :: Clause -> Bool
isSimpleClause (Id _) = True
isSimpleClause (Lifted (Id _)) = True
isSimpleClause (Ignore c) = isSimpleClause c
isSimpleClause _ = False

isNormalClause :: Clause -> Bool
isNormalClause (Star c Nothing) = isSimpleClause c
isNormalClause (Star c (Just c1)) = isSimpleClause c && isSimpleClause c1
isNormalClause (Plus c Nothing) = isSimpleClause c
isNormalClause (Plus c (Just c1)) = isSimpleClause c && isSimpleClause c1
isNormalClause (Opt c) = isSimpleClause c
isNormalClause (Alt cs) = all (\c -> case c of 
                                       Alt _ -> False
                                       _ -> isNormalClause c) cs
isNormalClause (Seq _ cs) = all isSimpleClause cs
isNormalClause _ = False
-}

processRuleOptions :: IRule -> Normalization ()
processRuleOptions r@IRule{getIDataTypeName=dtn, getIRuleName=rn, getIRuleOptions=ropts} = do
  let dtName = (maybe rn id dtn)
  mapM_ (\ opt -> case opt of
                    OShortcuts lst -> mapM_ (\ shortcut -> do
                                               addShortcut shortcut dtName
                                               return ()) lst
                    ) ropts

checkSimpleClause :: IClause -> Normalization SyntaxSimpleClause
checkSimpleClause c@(IId id) = return $ SSId id
checkSimpleClause c@(ILifted (IId id)) = return $ SSLifted id
checkSimpleClause c@(IIgnore c1) = do
  newC1 <- checkSimpleClause c1
  case newC1 of
    SSId id -> return $ SSIgnore id
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
checkNormalClause tc@(ISeq cs) = do
  c1 <- checkNormalClauseSeq tc
  return $ STAltOfSeq [c1]
checkNormalClause (ILifted c) = do
  c1 <- checkSimpleClause c
  case c1 of
    SSId id -> return $ STAltOfSeq [STSeq "" [SSLifted id]]
    _ -> error $ "Lifted cannot be applied to " ++ show c1
checkNormalClause (IIgnore c) = do
  c1 <- checkSimpleClause c
  case c1 of
    SSId id -> return $ STAltOfSeq [STSeq "" [SSIgnore id]]
    _ -> error $ "Ignore cannot be applied to " ++ show c1
checkNormalClause (IId id) = do
  return $ STAltOfSeq [STSeq "" [SSId id]]
checkNormalClause c = error $ "Wrong clause " ++ show c

checkNormalClauseSeq :: IClause -> Normalization STSeq
checkNormalClauseSeq (ISeq cs) = do
  cs1 <- mapM checkSimpleClause cs
  return $ STSeq "" cs1
checkNormalClauseSeq ic = do
  c1 <- checkSimpleClause ic
  return $ STSeq "" [c1]

normalizeRule :: IRule -> Normalization ()
normalizeRule r@IRule{getIDataTypeName=dtn, getIRuleName=rn, getIClause=cl} | not (isLexicalRule rn) = do
  processRuleOptions r
  newCl <- checkNormalClause cl
  addRuleWithQQ (maybe rn id dtn) rn newCl
normalizeRule r@IRule{getIDataTypeName=dtn, getIDataFunc=df, getIRuleName=rn, getIClause=cl} | (isLexicalRule rn) = do
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

doNM :: InitialGrammar -> Normalization ()
doNM grammar = do
  let grammar0 = everywhere (mkT removeOpts) grammar
  mapM_ normalizeRule $ getIRules grammar0
  postNormalizeGrammar

postNormalizeGroup :: (ID, [SyntaxRule]) -> Normalization (ID, [SyntaxRule])
postNormalizeGroup g@(id, [r]) = return g
postNormalizeGroup (id, rules) = do
  newRules <- mapM normRule rules
  return (id, newRules)
      where
          normRule r@(SyntaxRule _ (STAltOfSeq _)) = return r
          normRule (SyntaxRule rn cl) = do
                                   id <- extractSClause cl
                                   return (SyntaxRule rn (STAltOfSeq [STSeq "" [SSId id]]))

postNormalizeGrammar :: Normalization ()
postNormalizeGrammar = do
  rules <- gets (M.toList . normSRules)
  newRules <- mapM postNormalizeGroup rules
  modify (\ s@NormalizationState{ normSRules = nr } -> s{ normSRules = foldr (uncurry M.insert) nr newRules } )

addStartGroup :: NormalGrammar -> NormalGrammar
addStartGroup ng@NormalGrammar { getSyntaxRuleGroups = rules, getLexicalRules = tokens , getGrammarInfo = info } =
  let proxyRules = getProxyRules info
      (ruleToStartInfo, counter) = foldr
                                     (\el (map, counter) ->
                                        let typeName = getSDataTypeName el
                                        in
                                          if S.member typeName proxyRules
                                            then (map, counter)
                                            else
                                              (M.insert typeName 
                                                        ("tok_" ++ typeName ++ "_dummy_" ++ show counter)
                                                        map,
                                                counter + 1))
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
      startRule = head rules
    in
      ng { getSyntaxRuleGroups = startRule { getSRules = qqRule : getSRules startRule }: tail rules,
           getLexicalRules = newTokens ++ tokens,
           getGrammarInfo = info { getNameCounter = counter, getRuleToStartInfo = ruleToStartInfo }}

normalizeTopLevelClauses :: InitialGrammar -> NormalGrammar
normalizeTopLevelClauses grammar =
  let firstID = getIRuleName $ head $ getIRules grammar
      (_, NormalizationState nrs nls counter antiRules shortcuts proxyRules) =
        runState (doNM grammar) (NormalizationState M.empty [] 0 [] [] S.empty)
      firstRuleGroupRules = fromJust $ M.lookup firstID nrs
      nrs1 = M.delete firstID nrs
      firstGroup = SyntaxRuleGroup firstID firstRuleGroupRules
      otherGroups = map (\ (k,v) -> SyntaxRuleGroup k v) $ M.toList nrs1
      groups = firstGroup : otherGroups
    in addStartGroup $ NormalGrammar (getIGrammarName grammar) groups nls antiRules shortcuts (getImports grammar) (GrammarInfo (Just firstID) M.empty counter proxyRules)

data FillNameState = FillNameState { nameCtr :: Int, nameBase :: String, antiVarMap :: M.Map String String }
type FillName a = State FillNameState a

newConstructorName :: FillName String
newConstructorName = do
    n <- gets nameCtr
    b <- gets nameBase
    modify $ (\ s -> s{nameCtr = n + 1})
    return $ "Ctr__" ++ b ++ "__" ++  (show n)

fillConstructorName :: String -> STSeq -> FillName STSeq
fillConstructorName dataName (STSeq "" l) = do
    n <- newConstructorName
    return $ STSeq n l
fillConstructorName dataName seq = return seq 

fillConstructorNames :: NormalGrammar -> NormalGrammar
fillConstructorNames ng@NormalGrammar { getSyntaxRuleGroups = rules, getGrammarInfo = info } = 
    ng { getSyntaxRuleGroups = newrules, getGrammarInfo = info }
      where (newrules, newmap) = foldr ( \r (res, oldmap) ->
                                            let (rule, newmap) = doRename (getSDataTypeName r) r in (rule:res, M.union newmap oldmap)) ([], M.empty) rules
            doRename n dat = let (dat1, (FillNameState _ _ map)) = runState (everywhereM (mkM (fillConstructorName n)) dat) (FillNameState 0 n M.empty)
                               in (dat1, map)

removeOpts :: IClause -> IClause
removeOpts (IOpt c) = IAlt [ISeq [], ISeq [c]]
removeOpts a = a
