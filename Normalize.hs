module Normalize(normalizeTopLevelClauses, fillConstructorNames)
    where

import Parser
import Data.Char
import Data.Generics
import Data.Data
import Data.Maybe
import qualified Data.Map as Map

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
                                              newRules :: [NormalRule],
                                              nameCounter :: Int
                                             }
type Normalization a = State NormalizationState a

newName :: Normalization String
newName = do
  n <- gets nameCounter
  modify $ (\ s -> s{nameCounter = n + 1})
  return $ "Rule__" ++ (show n)

addRule :: NormalRule -> Normalization ()
addRule rule = do
  modify $ \s@NormalizationState{newRules = nr} -> s{ newRules = rule : nr}
  return ()

extractClause :: Clause -> Normalization Clause
extractClause cl = do
  ruleName <- newName
  cl1 <- checkNormalClause cl
  addRule $ case cl1 of
                Seq _ _ -> Rule ruleName "" ruleName cl1
                _       -> Rule ruleName "" ruleName (Seq "" [cl1])
  return (Id ruleName)

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

checkSimpleClause :: Clause -> Normalization Clause
checkSimpleClause c@(Id _) = return c
checkSimpleClause c@(Lifted (Id _)) = return c
checkSimpleClause c@(Ignore c1) = do
  newC1 <- checkSimpleClause c1
  return $ (Ignore newC1)
checkSimpleClause c = extractClause c

checkNormalClause :: Clause -> Normalization Clause
checkNormalClause (Star c mc) = do
  c1 <- checkSimpleClause c
  c2l <- mapM checkSimpleClause (maybeToList mc)
  return $ Star c1 (listToMaybe c2l)
checkNormalClause (Plus c mc) = do
  c1 <- checkSimpleClause c
  c2l <- mapM checkSimpleClause (maybeToList mc)
  return $ Plus c1 (listToMaybe c2l)
checkNormalClause (Opt c) = do
  c1 <- checkSimpleClause c
  return $ Opt c1
checkNormalClause (Alt [c]) = do
  c1 <- checkNormalClause c
  return c1
checkNormalClause (Alt cs) = do
  cs1 <- mapM (\c -> case c of 
                       Alt _ -> extractClause c
                       _ -> checkNormalClause c) cs
  return $ Alt cs1
checkNormalClause (Seq _ [c]) = do
  c1 <- checkNormalClause c
  return c1
checkNormalClause (Seq n cs) = do
  cs1 <- mapM checkSimpleClause cs
  return $ Seq n cs1
checkNormalClause (Lifted c) = do
  c1 <- checkSimpleClause c
  return $ Seq "" $ [Lifted c1]
checkNormalClause (Ignore c) = do
  c1 <- checkSimpleClause c
  return $ Seq "" $ [Ignore c1]
checkNormalClause id@(Id _) = do
  return $ Seq "" $ [id]
checkNormalClause c = checkSimpleClause c

normalizeRule :: NormalRule -> Normalization NormalRule
normalizeRule r@Rule{getRuleName=rn, getClause=cl} | not (isLexicalRule rn) = do
  newCl <- checkNormalClause cl
  return r{getClause = newCl}
normalizeRule r = return r

doNM :: NormalGrammar -> Normalization NormalGrammar
doNM grammar = do
  newGr <- everywhereM (mkM normalizeRule) grammar
  return newGr

normalizeTopLevelClauses :: NormalGrammar -> NormalGrammar
normalizeTopLevelClauses grammar = let (Grammar nm rules, NormalizationState nrs _) = runState (doNM grammar) (NormalizationState [] 0)
                                   in Grammar nm (rules ++ nrs)

data FillNameState = FillNameState { nameCtr :: Int, nameBase :: String }
type FillName a = State FillNameState a

newConstructorName :: FillName String
newConstructorName = do
    n <- gets nameCtr
    b <- gets nameBase
    modify $ (\ s -> s{nameCtr = n + 1})
    return $ "Ctr__" ++ b ++ "__" ++  (show n)

fillConstructorName :: Clause -> FillName Clause
fillConstructorName (Seq _ l) = do
    n <- newConstructorName
    return $ Seq n l
fillConstructorName cl = do
    return cl

fillConstructorNames :: NormalGrammar -> NormalGrammar
fillConstructorNames (Grammar name rules) = Grammar name (map renameRule rules)
    where renameRule r@Rule{ getRuleName = n, getClause = cl } | not (isLexicalRule n) = r { getClause=(doRename n cl) }
                                                               | otherwise = r
          doRename n cl = let (cl1, _) = runState (everywhereM (mkM fillConstructorName) cl) (FillNameState 0 n)
                          in cl1
