{-# LANGUAGE TemplateHaskell #-}
module SandboxQQ
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
import SandboxLexer
import SandboxParser

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

qqShortcuts = M.fromList [ ("sandbox","Sandbox")]

quoteSandboxExp :: Data.Data a => String -> (Sandbox -> a) -> String -> TH.ExpQ
quoteSandboxExp dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseSandbox $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToExpQ (const Nothing `Generics.extQ` antiSandboxExp) expr
quoteSandboxPat :: Data.Data a => String -> (Sandbox -> a) -> String -> TH.PatQ
quoteSandboxPat dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseSandbox $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToPatQ (const Nothing `Generics.extQ` antiSandboxPat) expr

antiSandboxExp :: Sandbox -> Maybe (TH.Q TH.Exp )
antiSandboxExp ( Anti_Sandbox v) = Just $ TH.varE (TH.mkName v)
antiSandboxExp _ = Nothing



antiSandboxPat :: Sandbox -> Maybe (TH.Q TH.Pat )
antiSandboxPat ( Anti_Sandbox v) = Just $ TH.varP (TH.mkName v)
antiSandboxPat _ = Nothing



quoteSandboxType s = return TH.ListT
quoteSandboxDecs s = return []

getSandbox ( Ctr__Sandbox__0 s) = s

sandbox :: QuasiQuoter
sandbox = QuasiQuoter (quoteSandboxExp "tok_Sandbox_dummy_0" getSandbox ) (quoteSandboxPat "tok_Sandbox_dummy_0" getSandbox ) quoteSandboxType quoteSandboxDecs

