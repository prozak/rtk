module StringLiterals (normalizeStringLiterals)
    where

import qualified GrammarParser as GP

import Frontend (mapGrammarRules, ruleName, strLitText)
import Syntax (isLexicalRule)
import Data.Char
import Data.Generics
import qualified Data.Map as Map

import Control.Monad.State.Strict hiding (lift)

translateStrLiteral :: String -> String
translateStrLiteral str = concat (map (\char -> case char of
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
                                                 c | isAlpha c -> [c]
                                                 c | isDigit c -> [c]
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

-- A string literal in a syntax rule becomes an ignored reference to a shared
-- keyword token; the replacement keeps the literal's source position, so
-- later normalization diagnostics still point at the literal.
normalizeClause :: GP.Clause -> StringLiteralsNormalization GP.Clause
normalizeClause (GP.Lit p s) = do
  tokName <- addStrLit (strLitText s)
  return $ GP.Ignored p (GP.Ref p (GP.Ident p tokName))
normalizeClause c = return c

normalizeRule :: GP.Rule -> StringLiteralsNormalization GP.Rule
normalizeRule r@GP.Anti_Rule{}               = return r
normalizeRule r | isLexicalRule (ruleName r) = return r
normalizeRule (GP.RuleWithOptions p opts r)  = GP.RuleWithOptions p opts <$> normalizeRule r
normalizeRule (GP.RuleSimple p n c)          = GP.RuleSimple p n <$> normalizeClauseTree c
normalizeRule (GP.RuleTyped p t n c)         = GP.RuleTyped p t n <$> normalizeClauseTree c
normalizeRule (GP.RuleTypedFunc p t f n c)   = GP.RuleTypedFunc p t f n <$> normalizeClauseTree c
normalizeRule (GP.RuleFunc p f n c)          = GP.RuleFunc p f n <$> normalizeClauseTree c

normalizeClauseTree :: GP.Clause -> StringLiteralsNormalization GP.Clause
normalizeClauseTree = everywhereM (mkM normalizeClause)

doSLNM :: GP.Grammar -> StringLiteralsNormalization GP.Grammar
doSLNM (GP.GrammarDef p n rules)         = GP.GrammarDef p n <$> mapM normalizeRule rules
doSLNM (GP.GrammarImports p n imp rules) = GP.GrammarImports p n imp <$> mapM normalizeRule rules
doSLNM g                                 = return g

-- | Replace every string literal in the syntax rules by an ignored keyword
-- token (one shared token per distinct literal) and append the synthesized
-- keyword rules - Keyword-typed lexical rules whose clause is the literal.
normalizeStringLiterals :: GP.Grammar -> GP.Grammar
normalizeStringLiterals grammar =
    let (grammar1, StringLiteralsNormalizationState m _) =
            runState (doSLNM grammar) (StringLiteralsNormalizationState Map.empty 0)
        slRules sm = map (uncurry keywordRule) $ Map.toList sm
    in mapGrammarRules (++ slRules m) grammar1

keywordRule :: String -> String -> GP.Rule
keywordRule lit tokName =
    GP.RuleTypedFunc np (ident "Keyword") (ident "id") (ident tokName)
                     (GP.Lit np (GP.Str np lit))
  where np    = GP.rtkNoPos
        ident = GP.Ident np
