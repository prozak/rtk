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

replaceAllPatterns1 :: String -> String
replaceAllPatterns1 str = let (pre, match, post) = str =~ qqPattern :: (String, String, String)
                          in if match == ""
                              then pre
                              else let varName = init $ tail match
                                       addSym = last match
                                       ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName
                                       rule = case ruleVariants of
                                                [] -> error $ "Unknown shortcut for " ++ varName
                                                (rule : _) -> rule
                                   in pre ++ ('$' : rule ++ ":") ++ varName ++ (replaceAllPatterns1 $ addSym : post)

-- Add ' ' at the end, so regex can match variable in the end of the string
replaceAllPatterns :: String -> String
replaceAllPatterns str = init $ replaceAllPatterns1 (str ++ " ")

qqShortcuts = M.fromList [ ("a","A"),("b","B"),("c","C"),("d","D"),("e","E"),("f1","F1"),("f2","F2"),("f3","F3"),("f4","F4"),("f5","F5"),("g","G")]

quoteT1Exp :: Data.Data a => String -> (A -> a) -> String -> TH.ExpQ
quoteT1Exp dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseT1 $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToExpQ (const Nothing `Generics.extQ` antiAExp `Generics.extQ` antiBExp `Generics.extQ` antiDExp `Generics.extQ` antiEExp `Generics.extQ` antiF4Exp `Generics.extQ` antiGExp) expr
quoteT1Pat :: Data.Data a => String -> (A -> a) -> String -> TH.PatQ
quoteT1Pat dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseT1 $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToPatQ (const Nothing `Generics.extQ` antiAPat `Generics.extQ` antiBPat `Generics.extQ` antiDPat `Generics.extQ` antiEPat `Generics.extQ` antiF4Pat `Generics.extQ` antiGPat) expr

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

getA ( Ctr__A__0 s) = s

a :: QuasiQuoter
a = QuasiQuoter (quoteT1Exp "tok_A_dummy_19" getA ) (quoteT1Pat "tok_A_dummy_19" getA ) quoteT1Type quoteT1Decs

getB ( Ctr__A__1 s) = s

b :: QuasiQuoter
b = QuasiQuoter (quoteT1Exp "tok_B_dummy_18" getB ) (quoteT1Pat "tok_B_dummy_18" getB ) quoteT1Type quoteT1Decs

getC ( Ctr__A__2 s) = s

c :: QuasiQuoter
c = QuasiQuoter (quoteT1Exp "tok_C_dummy_17" getC ) (quoteT1Pat "tok_C_dummy_17" getC ) quoteT1Type quoteT1Decs

getD ( Ctr__A__3 s) = s

d :: QuasiQuoter
d = QuasiQuoter (quoteT1Exp "tok_D_dummy_16" getD ) (quoteT1Pat "tok_D_dummy_16" getD ) quoteT1Type quoteT1Decs

getE ( Ctr__A__4 s) = s

e :: QuasiQuoter
e = QuasiQuoter (quoteT1Exp "tok_E_dummy_15" getE ) (quoteT1Pat "tok_E_dummy_15" getE ) quoteT1Type quoteT1Decs

getF1 ( Ctr__A__5 s) = s

f1 :: QuasiQuoter
f1 = QuasiQuoter (quoteT1Exp "tok_F1_dummy_14" getF1 ) (quoteT1Pat "tok_F1_dummy_14" getF1 ) quoteT1Type quoteT1Decs

getF2 ( Ctr__A__6 s) = s

f2 :: QuasiQuoter
f2 = QuasiQuoter (quoteT1Exp "tok_F2_dummy_13" getF2 ) (quoteT1Pat "tok_F2_dummy_13" getF2 ) quoteT1Type quoteT1Decs

getF3 ( Ctr__A__7 s) = s

f3 :: QuasiQuoter
f3 = QuasiQuoter (quoteT1Exp "tok_F3_dummy_12" getF3 ) (quoteT1Pat "tok_F3_dummy_12" getF3 ) quoteT1Type quoteT1Decs

getF4 ( Ctr__A__8 s) = s

f4 :: QuasiQuoter
f4 = QuasiQuoter (quoteT1Exp "tok_F4_dummy_11" getF4 ) (quoteT1Pat "tok_F4_dummy_11" getF4 ) quoteT1Type quoteT1Decs

getF5 ( Ctr__A__9 s) = s

f5 :: QuasiQuoter
f5 = QuasiQuoter (quoteT1Exp "tok_F5_dummy_10" getF5 ) (quoteT1Pat "tok_F5_dummy_10" getF5 ) quoteT1Type quoteT1Decs

getG ( Ctr__A__10 s) = s

g :: QuasiQuoter
g = QuasiQuoter (quoteT1Exp "tok_G_dummy_9" getG ) (quoteT1Pat "tok_G_dummy_9" getG ) quoteT1Type quoteT1Decs

