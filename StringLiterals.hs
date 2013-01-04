module StringLiterals (normalizeStringLiterals)
    where

import Parser
import Data.Char
import Data.Generics
import Data.Data
import qualified Data.Map as Map

import Control.Monad.State.Strict hiding (lift)

translateStrLiteral :: String -> String
translateStrLiteral str = concat (map (\chr -> case chr of
                                                 '+' -> "_plus_"
                                                 '-' -> "_minus_"
                                                 '.' -> "_dot_"
                                                 ',' -> "_coma_"
                                                 '[' -> "_sq_bkt_l_"
                                                 ']' -> "_sq_bkt_r_"
                                                 ';' -> "_semi_"
                                                 ':' -> "_colon_"
                                                 '=' -> "_eql_"
                                                 '*' -> "_star_"
                                                 '|' -> "_pipe_"
                                                 '$' -> "_dollar_"
                                                 '!' -> "_exclamation_"
                                                 '~' -> "_tilde_"
                                                 '(' -> "_lparen_"
                                                 ')' -> "_rparen_"
                                                 c | isAlpha c -> [chr]
                                                 c | isDigit c -> [chr]
                                                 _ -> "_symbol_")
                                  str)

type StringLiteralsMap = Map.Map String String

data StringLiteralsNormalizationState = StringLiteralsNormalizationState {
                                                                          slnMap :: StringLiteralsMap,
                                                                          nameCounter :: Int
                                                                         }
type StringLiteralsNormalization a = State StringLiteralsNormalizationState a

newTokName :: String -> StringLiteralsNormalization String
newTokName str = do
  n <- gets nameCounter
  modify $ (\ s -> s{nameCounter = n + 1})
  return $ "tok_" ++ (translateStrLiteral str) ++ "_" ++ (show n)

addStrLit :: String -> StringLiteralsNormalization String
addStrLit str = do
  m <- gets slnMap
  case Map.lookup str m of
    Nothing -> do
      tokName <- newTokName str
      modify $ \s -> s{slnMap = Map.insert str tokName m}
      return tokName
    Just tokName -> return tokName

normalizeClause :: IClause -> StringLiteralsNormalization IClause
normalizeClause (IStrLit str) = do
  tokName <- addStrLit str
  return $ IIgnore (IId tokName)
normalizeClause c = return c

normalizeRule :: IRule -> StringLiteralsNormalization IRule
normalizeRule r@IRule{getIRuleName=rn, getIClause=cl} | not (isLexicalRule rn) = do
  newCl <- everywhereM (mkM normalizeClause) cl
  return r{getIClause = newCl}
normalizeRule r = return r

doSLNM :: InitialGrammar -> StringLiteralsNormalization InitialGrammar
doSLNM grammar = do
  newGr <- everywhereM (mkM normalizeRule) grammar
  return newGr

normalizeStringLiterals :: InitialGrammar -> InitialGrammar
normalizeStringLiterals grammar = let (InitialGrammar nm imports rules, StringLiteralsNormalizationState m _) = runState (doSLNM grammar) (StringLiteralsNormalizationState Map.empty 0)
                                      slRules sm = map (\ (k, v) -> IRule (Just "Keyword") (Just "id") v (IStrLit k) []) $ Map.toList sm
                                  in InitialGrammar nm imports (rules ++ slRules m)


