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
                                              nameCounter :: Int
                                             }
type Normalization a = State NormalizationState a

newName :: Normalization String
newName = do
  n <- gets nameCounter
  modify $ (\ s -> s{nameCounter = n + 1})
  return $ "Rule__" ++ (show n)

addRule :: ID -> ID -> SyntaxTopClause -> Normalization ()
addRule tdName ruleName clause = do
  let doAdd rs = Just $ (SyntaxRule ruleName clause):(maybe [] id rs)
  modify (\ s@NormalizationState{ normSRules = nr } -> s{ normSRules = M.alter doAdd tdName nr } )
  return ()

addLexicalRule :: LexicalRule -> Normalization ()
addLexicalRule lr = do
  modify $ \s@NormalizationState{ normLRules = nr} -> s{ normLRules = lr:nr }
  return ()

extractClause :: IClause -> Normalization ID
extractClause cl = do
  ruleName <- newName
  cl1 <- checkNormalClause cl
  addRule ruleName ruleName cl1
  return $ ruleName

extractSClause :: SyntaxTopClause -> Normalization ID
extractSClause cl = do
  ruleName <- newName
  addRule ruleName ruleName cl
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
  newCl <- checkNormalClause cl
  addRule (maybe rn id dtn) rn newCl
normalizeRule r@IRule{getIDataTypeName=dtn, getIDataFunc=df, getIRuleName=rn, getIClause=cl} | (isLexicalRule rn) = do
  let (dtn1, df1) = case (dtn, df) of
                      (Nothing, Nothing) -> ("String", "id")
                      (Just d,  Nothing) -> (d,        "read")
                      (Just d,   Just f) -> (d,        f)
                      (Nothing,  Just f) -> ("String", f)
  addLexicalRule $ LexicalRule dtn1 df1 rn cl

doNM :: InitialGrammar -> Normalization ()
doNM grammar = do
  mapM_ normalizeRule $ getIRules grammar
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
  let (ruleToStartInfo, counter) = foldr
                                     (\el (map, counter) -> 
                                        (M.insert (getSDataTypeName el) ("tok__" ++ getSDataTypeName el ++ "__dummy__" ++ show counter) map, counter + 1))
                                     (M.empty, getNameCounter info)
                                     rules
      mainRuleClause = STSeq "" [SSId $ getSDataTypeName $ head rules]
      rulesClauses = map (\s ->
                           let dummy = SSIgnore (fromJust (M.lookup (getSDataTypeName s) ruleToStartInfo))
                           in 
                           STSeq "" [dummy,
                                     SSId $ getSDataTypeName s,
                                     dummy]) rules
      newTokens = map (\(_, name) -> LexicalRule { getLRuleDataType = "Keyword",
                                                   getLRuleFunc = "",
                                                   getLRuleName = name, getLClause = (IStrLit name)}) $ M.toList ruleToStartInfo
      startRuleName = "Start"
      startRuleGroup = SyntaxRuleGroup startRuleName [SyntaxRule startRuleName $ STAltOfSeq $ mainRuleClause : rulesClauses]
    in
      ng { getSyntaxRuleGroups = startRuleGroup : rules,
           getLexicalRules = newTokens ++ tokens,
           getGrammarInfo = info { getStartRuleName = Just startRuleName, getNameCounter = counter, getRuleToStartInfo = ruleToStartInfo }}

type QQState a = State NormalGrammar a

genQQName :: String -> QQState String
genQQName str =
  do
    counter <- gets (getNameCounter . getGrammarInfo)
    modify (\s@NormalGrammar{ getGrammarInfo = info } -> s { getGrammarInfo = info { getNameCounter = counter + 1 }})
    return $ "tok__" ++ str ++ "__" ++ (show counter)

addQQVarsToGrammarHelper :: QQState ()
addQQVarsToGrammarHelper = do
  tokens <- gets getLexicalRules
  let colonFound = L.find (\token -> case token of
                                       LexicalRule "Keyword" "id" _ (IStrLit ":") -> True
                                       _ -> False)
                     tokens
  let idFound = L.find (\token -> case token of
                                    LexicalRule "String" "id" _ (IAlt [ISeq [IRegExpLit "a-zA-Z",
                                                                             IStar (IRegExpLit "A-Za-z0-9_") Nothing]]) -> True
                                    _ -> False)
                  tokens
  colonName <- genQQName "colon"
  let colon = case colonFound of
                Just token -> token
                Nothing -> LexicalRule "Keyword" "id" colonName (IStrLit ":")
  idName <- genQQName "id"
  let id = case idFound of
             Just token -> token
             Nothing -> LexicalRule "String" "id" idName (IAlt [ISeq [IRegExpLit "a-zA-Z",
                                                                IStar (IRegExpLit "a-zA-Z0-9_") Nothing]])
  case colonFound of
    Nothing -> modify (\s@NormalGrammar{getLexicalRules = tokens} -> s{getLexicalRules = colon : tokens})
    _ -> return ()
  case idFound of
    Nothing -> modify (\s@NormalGrammar{getLexicalRules = tokens} -> s{getLexicalRules = tokens ++ [id]})
    _ -> return ()
  ruleGs <- gets getSyntaxRuleGroups
  newRuleGs <-
    mapM (\(SyntaxRuleGroup name@(s : rest) rules) -> do
             let lowername = ((toLower s) : rest)
             termKindName <- genQQName lowername
             let termKind = LexicalRule "Keyword" "id" termKindName (IStrLit $ "$" ++ lowername)
             modify (\s@NormalGrammar{getLexicalRules = tokens} -> s {getLexicalRules = termKind : tokens})
             return (SyntaxRuleGroup name
                                     (map (\sr@(SyntaxRule name topclause) ->
                                             let toAdd = STSeq "$anti$" [SSIgnore (getLRuleName termKind),
                                                                         SSIgnore (getLRuleName colon),
                                                                         SSId (getLRuleName id)]
                                               in
                                               case topclause of
                                                 STAltOfSeq altseqs ->
                                                   case L.find (\(STSeq _ ssc) -> isJust $ L.find (\sc -> case sc of
                                                                                                    SSLifted _ -> True
                                                                                                    _ -> False) ssc) altseqs of
                                                     Just _ -> sr
                                                     Nothing -> SyntaxRule name (STAltOfSeq $ toAdd : altseqs)
                                                 {-STOpt ssc ->
                                                   SyntaxRule name (STAltOfSeq $ [toAdd, STSeq "" [], STSeq "" [ssc]])-}
                                                 _ -> sr)
                                      rules))) $ tail ruleGs
  modify (\s@NormalGrammar{getSyntaxRuleGroups = ruleGs} -> s {getSyntaxRuleGroups = head ruleGs : newRuleGs})

addQQVarsToGrammar :: NormalGrammar -> NormalGrammar
addQQVarsToGrammar ng =
  let (_, nng) = runState addQQVarsToGrammarHelper ng
    in nng

normalizeTopLevelClauses :: InitialGrammar -> NormalGrammar
normalizeTopLevelClauses grammar =
  let firstID = getIRuleName $ head $ getIRules grammar
      (_, NormalizationState nrs nls counter) = runState (doNM grammar) (NormalizationState M.empty [] 0)
      firstRuleGroupRules = fromJust $ M.lookup firstID nrs
      nrs1 = M.delete firstID nrs
      firstGroup = SyntaxRuleGroup firstID firstRuleGroupRules
      otherGroups = map (\ (k,v) -> SyntaxRuleGroup k v) $ M.toList nrs1
      groups = firstGroup : otherGroups
    in addQQVarsToGrammar $ addStartGroup $ NormalGrammar (getIGrammarName grammar) groups nls (GrammarInfo Nothing M.empty M.empty counter)
    --in addStartGroup $ NormalGrammar (getIGrammarName grammar) groups nls (GrammarInfo Nothing M.empty M.empty counter)

data FillNameState = FillNameState { nameCtr :: Int, nameBase :: String, antiVarMap :: M.Map String String }
type FillName a = State FillNameState a

newConstructorName :: FillName String
newConstructorName = do
    n <- gets nameCtr
    b <- gets nameBase
    modify $ (\ s -> s{nameCtr = n + 1})
    return $ "Ctr__" ++ b ++ "__" ++  (show n)

fillConstructorName :: String -> STSeq -> FillName STSeq
fillConstructorName dataName (STSeq "$anti$" l) = do
    n <- newConstructorName
    modify $ \s -> s{ antiVarMap = M.insert dataName n $ antiVarMap s}
    return $ STSeq n l
fillConstructorName dataName (STSeq _ l) = do
    n <- newConstructorName
    return $ STSeq n l

fillConstructorNames :: NormalGrammar -> NormalGrammar
fillConstructorNames ng@NormalGrammar { getSyntaxRuleGroups = rules, getGrammarInfo = info } = 
    ng { getSyntaxRuleGroups = newrules, getGrammarInfo = info { getRuleToAntiInfo = newmap }}
      where (newrules, newmap) = foldr ( \r (res, oldmap) ->
                                            let (rule, newmap) = doRename (getSDataTypeName r) r in (rule:res, M.union newmap oldmap)) ([], M.empty) rules
            doRename n dat = let (dat1, (FillNameState _ _ map)) = runState (everywhereM (mkM (fillConstructorName n)) dat) (FillNameState 0 n M.empty)
                               in (dat1, map)
