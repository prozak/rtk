{-# LANGUAGE TemplateHaskell #-}
module T1QQ
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
import T1Lexer
import T1Parser

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

qqShortcuts = M.fromList [ ("a","A"),("b","B"),("c","C"),("d","D"),("e","E"),("f1","F1"),("f2","F2"),("f3","F3"),("f4","F4"),("f5","F5"),("g","G")]

-- A quasi-quote pattern must match an AST parsed from anywhere in a source
-- file, while the pattern itself was parsed from the quote body - so every
-- RtkPos position field becomes a wildcard in generated patterns.
-- (Expressions need no special case: the compile-time position they embed
-- is equality-transparent.)
rtkPosWildPat :: RtkPos -> Maybe (TH.Q TH.Pat)
rtkPosWildPat _ = Just TH.wildP

quoteT1Exp :: Data.Data a => String -> (A -> a) -> String -> TH.ExpQ
quoteT1Exp dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseT1 of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToExpQ (const Nothing `Generics.extQ` antiAExp `Generics.extQ` antiBExp `Generics.extQ` antiDExp `Generics.extQ` antiEExp `Generics.extQ` antiF4Exp `Generics.extQ` antiGExp) expr
quoteT1Pat :: Data.Data a => String -> (A -> a) -> String -> TH.PatQ
quoteT1Pat dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseT1 of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToPatQ (const Nothing `Generics.extQ` rtkPosWildPat `Generics.extQ` antiAPat `Generics.extQ` antiBPat `Generics.extQ` antiDPat `Generics.extQ` antiEPat `Generics.extQ` antiF4Pat `Generics.extQ` antiGPat) expr

antiGExp :: G -> Maybe (TH.Q TH.Exp )
antiGExp ( Anti_G v) = Just $ TH.varE (TH.mkName v)
antiGExp _ = Nothing


antiF4Exp :: F4 -> Maybe (TH.Q TH.Exp )
antiF4Exp ( Anti_F4 v) = Just $ TH.varE (TH.mkName v)
antiF4Exp _ = Nothing


antiEExp :: E -> Maybe (TH.Q TH.Exp )
antiEExp ( Anti_E v) = Just $ TH.varE (TH.mkName v)
antiEExp _ = Nothing


antiDExp :: D -> Maybe (TH.Q TH.Exp )
antiDExp ( Anti_D v) = Just $ TH.varE (TH.mkName v)
antiDExp _ = Nothing


antiBExp :: B -> Maybe (TH.Q TH.Exp )
antiBExp ( Anti_B v) = Just $ TH.varE (TH.mkName v)
antiBExp _ = Nothing


antiAExp :: A -> Maybe (TH.Q TH.Exp )
antiAExp ( Anti_A v) = Just $ TH.varE (TH.mkName v)
antiAExp _ = Nothing



antiGPat :: G -> Maybe (TH.Q TH.Pat )
antiGPat ( Anti_G v) = Just $ TH.varP (TH.mkName v)
antiGPat _ = Nothing


antiF4Pat :: F4 -> Maybe (TH.Q TH.Pat )
antiF4Pat ( Anti_F4 v) = Just $ TH.varP (TH.mkName v)
antiF4Pat _ = Nothing


antiEPat :: E -> Maybe (TH.Q TH.Pat )
antiEPat ( Anti_E v) = Just $ TH.varP (TH.mkName v)
antiEPat _ = Nothing


antiDPat :: D -> Maybe (TH.Q TH.Pat )
antiDPat ( Anti_D v) = Just $ TH.varP (TH.mkName v)
antiDPat _ = Nothing


antiBPat :: B -> Maybe (TH.Q TH.Pat )
antiBPat ( Anti_B v) = Just $ TH.varP (TH.mkName v)
antiBPat _ = Nothing


antiAPat :: A -> Maybe (TH.Q TH.Pat )
antiAPat ( Anti_A v) = Just $ TH.varP (TH.mkName v)
antiAPat _ = Nothing



quoteT1Type s = return TH.ListT
quoteT1Decs s = return []

getA ( Ctr__A__0 _ s) = s

a :: QuasiQuoter
a = QuasiQuoter (quoteT1Exp "tok_A_dummy_19" getA ) (quoteT1Pat "tok_A_dummy_19" getA ) quoteT1Type quoteT1Decs

getB ( Ctr__A__1 _ s) = s

b :: QuasiQuoter
b = QuasiQuoter (quoteT1Exp "tok_B_dummy_18" getB ) (quoteT1Pat "tok_B_dummy_18" getB ) quoteT1Type quoteT1Decs

getC ( Ctr__A__2 _ s) = s

c :: QuasiQuoter
c = QuasiQuoter (quoteT1Exp "tok_C_dummy_17" getC ) (quoteT1Pat "tok_C_dummy_17" getC ) quoteT1Type quoteT1Decs

getD ( Ctr__A__3 _ s) = s

d :: QuasiQuoter
d = QuasiQuoter (quoteT1Exp "tok_D_dummy_16" getD ) (quoteT1Pat "tok_D_dummy_16" getD ) quoteT1Type quoteT1Decs

getE ( Ctr__A__4 _ s) = s

e :: QuasiQuoter
e = QuasiQuoter (quoteT1Exp "tok_E_dummy_15" getE ) (quoteT1Pat "tok_E_dummy_15" getE ) quoteT1Type quoteT1Decs

getF1 ( Ctr__A__5 _ s) = s

f1 :: QuasiQuoter
f1 = QuasiQuoter (quoteT1Exp "tok_F1_dummy_14" getF1 ) (quoteT1Pat "tok_F1_dummy_14" getF1 ) quoteT1Type quoteT1Decs

getF2 ( Ctr__A__6 _ s) = s

f2 :: QuasiQuoter
f2 = QuasiQuoter (quoteT1Exp "tok_F2_dummy_13" getF2 ) (quoteT1Pat "tok_F2_dummy_13" getF2 ) quoteT1Type quoteT1Decs

getF3 ( Ctr__A__7 _ s) = s

f3 :: QuasiQuoter
f3 = QuasiQuoter (quoteT1Exp "tok_F3_dummy_12" getF3 ) (quoteT1Pat "tok_F3_dummy_12" getF3 ) quoteT1Type quoteT1Decs

getF4 ( Ctr__A__8 _ s) = s

f4 :: QuasiQuoter
f4 = QuasiQuoter (quoteT1Exp "tok_F4_dummy_11" getF4 ) (quoteT1Pat "tok_F4_dummy_11" getF4 ) quoteT1Type quoteT1Decs

getF5 ( Ctr__A__9 _ s) = s

f5 :: QuasiQuoter
f5 = QuasiQuoter (quoteT1Exp "tok_F5_dummy_10" getF5 ) (quoteT1Pat "tok_F5_dummy_10" getF5 ) quoteT1Type quoteT1Decs

getG ( Ctr__A__10 _ s) = s

g :: QuasiQuoter
g = QuasiQuoter (quoteT1Exp "tok_G_dummy_9" getG ) (quoteT1Pat "tok_G_dummy_9" getG ) quoteT1Type quoteT1Decs

