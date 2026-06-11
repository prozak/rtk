{-# LANGUAGE TemplateHaskell #-}
module PQQ
where

import Text.Regex.Posix
import Text.Regex.Base
import qualified Data.Map as M
import Data.List
import Data.Maybe
import qualified Data.Generics as Generics
import qualified Data.Data as Data
import qualified Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import PLexer
import PParser

qqPattern = "\\$[A-Za-z_][A-Za-z_0-9]*[^A-Za-z_0-9:]"

qqShortcuts :: M.Map String String

-- A $name metavariable is rewritten to $Type:name using the qqShortcuts
-- table below. The rewrite is purely textual, so it would also fire inside
-- the quoted language's own string literals: write $$name there to escape
-- it and get the literal text $name. Each '$$' pair directly before a
-- metavariable stands for one literal '$' (so $$$x is a literal '$'
-- followed by the metavariable $x). A '$' not followed by an identifier is
-- never rewritten and needs no escape.
replaceAllPatterns1 :: String -> Either String String
replaceAllPatterns1 str = let (pre, match, post) = str =~ qqPattern :: (String, String, String)
                          in if match == ""
                              then Right pre
                              else let varName = init $ tail match
                                       addSym = last match
                                       escCount = length $ takeWhile (== '$') $ reverse pre
                                       keptPre = take (length pre - escCount) pre ++ replicate (div escCount 2) '$'
                                       ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName
                                   in if odd escCount
                                       then (\rest -> keptPre ++ ('$' : varName) ++ rest) <$> (replaceAllPatterns1 $ addSym : post)
                                       else case ruleVariants of
                                              [] -> Left $ unlines
                                                      [ "Unknown metavariable $" ++ varName ++ " in quasi-quote:"
                                                      , "no prefix of '" ++ varName ++ "' is a known shortcut. Known shortcuts:"
                                                      , "  " ++ intercalate ", " (M.keys qqShortcuts)
                                                      , "To include the literal text $" ++ varName ++ " in the quoted code"
                                                      , "(e.g. inside a string literal), escape it as $$" ++ varName ++ "." ]
                                              (rule : _) -> (\rest -> keptPre ++ ('$' : rule ++ ":") ++ varName ++ rest) <$> (replaceAllPatterns1 $ addSym : post)

-- Add ' ' at the end, so regex can match variable in the end of the string
replaceAllPatterns :: String -> Either String String
replaceAllPatterns str = init <$> replaceAllPatterns1 (str ++ " ")

qqShortcuts = M.fromList [ ("p","P"),("e","E"),("id","Id"),("op1","Op1"),("op2","Op2")]

-- A quasi-quote pattern must match an AST parsed from anywhere in a source
-- file, while the pattern itself was parsed from the quote body - so every
-- RtkPos position field becomes a wildcard in generated patterns.
-- (Expressions need no special case: the compile-time position they embed
-- is equality-transparent.)
rtkPosWildPat :: RtkPos -> Maybe (TH.Q TH.Pat)
rtkPosWildPat _ = Just TH.wildP

quotePExp :: Data.Data a => String -> (P -> a) -> String -> TH.ExpQ
quotePExp dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseP of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToExpQ (const Nothing `Generics.extQ` antiPExp `Generics.extQ` antiEExp `Generics.extQ` antiOp1Exp `Generics.extQ` antiOp2Exp `Generics.extQ` antiIdExp) expr
quotePPat :: Data.Data a => String -> (P -> a) -> String -> TH.PatQ
quotePPat dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseP of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToPatQ (const Nothing `Generics.extQ` rtkPosWildPat `Generics.extQ` antiPPat `Generics.extQ` antiEPat `Generics.extQ` antiOp1Pat `Generics.extQ` antiOp2Pat `Generics.extQ` antiIdPat) expr

antiIdExp :: Id -> Maybe (TH.Q TH.Exp )
antiIdExp ( Anti_Id v) = Just $ TH.varE (TH.mkName v)
antiIdExp _ = Nothing


antiOp2Exp :: Op2 -> Maybe (TH.Q TH.Exp )
antiOp2Exp ( Anti_Op2 v) = Just $ TH.varE (TH.mkName v)
antiOp2Exp _ = Nothing


antiOp1Exp :: Op1 -> Maybe (TH.Q TH.Exp )
antiOp1Exp ( Anti_Op1 v) = Just $ TH.varE (TH.mkName v)
antiOp1Exp _ = Nothing


antiEExp :: E -> Maybe (TH.Q TH.Exp )
antiEExp ( Anti_E v) = Just $ TH.varE (TH.mkName v)
antiEExp _ = Nothing


antiPExp :: P -> Maybe (TH.Q TH.Exp )
antiPExp ( Anti_P v) = Just $ TH.varE (TH.mkName v)
antiPExp _ = Nothing



antiIdPat :: Id -> Maybe (TH.Q TH.Pat )
antiIdPat ( Anti_Id v) = Just $ TH.varP (TH.mkName v)
antiIdPat _ = Nothing


antiOp2Pat :: Op2 -> Maybe (TH.Q TH.Pat )
antiOp2Pat ( Anti_Op2 v) = Just $ TH.varP (TH.mkName v)
antiOp2Pat _ = Nothing


antiOp1Pat :: Op1 -> Maybe (TH.Q TH.Pat )
antiOp1Pat ( Anti_Op1 v) = Just $ TH.varP (TH.mkName v)
antiOp1Pat _ = Nothing


antiEPat :: E -> Maybe (TH.Q TH.Pat )
antiEPat ( Anti_E v) = Just $ TH.varP (TH.mkName v)
antiEPat _ = Nothing


antiPPat :: P -> Maybe (TH.Q TH.Pat )
antiPPat ( Anti_P v) = Just $ TH.varP (TH.mkName v)
antiPPat _ = Nothing



quotePType s = return TH.ListT
quotePDecs s = return []

getP ( Ctr__P__0 _ s) = s

p :: QuasiQuoter
p = QuasiQuoter (quotePExp "tok_P_dummy_4" getP ) (quotePPat "tok_P_dummy_4" getP ) quotePType quotePDecs

getE ( Ctr__P__1 _ s) = s

e :: QuasiQuoter
e = QuasiQuoter (quotePExp "tok_E_dummy_3" getE ) (quotePPat "tok_E_dummy_3" getE ) quotePType quotePDecs

getId ( Ctr__P__2 _ s) = s

id :: QuasiQuoter
id = QuasiQuoter (quotePExp "tok_Id_dummy_2" getId ) (quotePPat "tok_Id_dummy_2" getId ) quotePType quotePDecs

getOp1 ( Ctr__P__3 _ s) = s

op1 :: QuasiQuoter
op1 = QuasiQuoter (quotePExp "tok_Op1_dummy_1" getOp1 ) (quotePPat "tok_Op1_dummy_1" getOp1 ) quotePType quotePDecs

getOp2 ( Ctr__P__4 _ s) = s

op2 :: QuasiQuoter
op2 = QuasiQuoter (quotePExp "tok_Op2_dummy_0" getOp2 ) (quotePPat "tok_Op2_dummy_0" getOp2 ) quotePType quotePDecs

