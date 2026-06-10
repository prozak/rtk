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

qqShortcuts = M.fromList [ ("sandbox","Sandbox")]

quoteSandboxExp :: Data.Data a => String -> (Sandbox -> a) -> String -> TH.ExpQ
quoteSandboxExp dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseSandbox of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
  dataToExpQ (const Nothing `Generics.extQ` antiSandboxExp) expr
quoteSandboxPat :: Data.Data a => String -> (Sandbox -> a) -> String -> TH.PatQ
quoteSandboxPat dummy func s = do
  s1 <- either fail return (replaceAllPatterns s)
  ast <- case scanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy) >>= parseSandbox of
           Left err -> fail err
           Right a -> return a
  let expr = func ast
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

