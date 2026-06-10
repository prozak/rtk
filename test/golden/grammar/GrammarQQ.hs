{-# LANGUAGE TemplateHaskell #-}
module GrammarQQ
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
import GrammarLexer
import GrammarParser

qqPattern = "\\$[A-Za-z_][A-Za-z_0-9]*[^A-Za-z_0-9:]"

qqShortcuts :: M.Map String String

-- A $name metavariable is rewritten to $Type:name using the qqShortcuts
-- table below. The rewrite is purely textual, so it would also fire inside
-- the quoted language's own string literals: write $$name there to escape
-- it and get the literal text $name. Each '$$' pair directly before a
-- metavariable stands for one literal '$' (so $$$x is a literal '$'
-- followed by the metavariable $x). A '$' not followed by an identifier is
-- never rewritten and needs no escape.
replaceAllPatterns1 :: String -> String
replaceAllPatterns1 str = let (pre, match, post) = str =~ qqPattern :: (String, String, String)
                          in if match == ""
                              then pre
                              else let varName = init $ tail match
                                       addSym = last match
                                       escCount = length $ takeWhile (== '$') $ reverse pre
                                       keptPre = take (length pre - escCount) pre ++ replicate (div escCount 2) '$'
                                       ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts) $ reverse $ inits varName
                                       rule = case ruleVariants of
                                                [] -> error $ unlines
                                                        [ "Unknown metavariable $" ++ varName ++ " in quasi-quote:"
                                                        , "no prefix of '" ++ varName ++ "' is a known shortcut. Known shortcuts:"
                                                        , "  " ++ intercalate ", " (M.keys qqShortcuts)
                                                        , "To include the literal text $" ++ varName ++ " in the quoted code"
                                                        , "(e.g. inside a string literal), escape it as $$" ++ varName ++ "." ]
                                                (rule : _) -> rule
                                   in if odd escCount
                                       then keptPre ++ ('$' : varName) ++ (replaceAllPatterns1 $ addSym : post)
                                       else keptPre ++ ('$' : rule ++ ":") ++ varName ++ (replaceAllPatterns1 $ addSym : post)

-- Add ' ' at the end, so regex can match variable in the end of the string
replaceAllPatterns :: String -> String
replaceAllPatterns str = init $ replaceAllPatterns1 (str ++ " ")

qqShortcuts = M.fromList [ ("grammar","Grammar"),("clause","Clause"),("idList","IdList"),("importsOpt","ImportsOpt"),("name","Name"),("optDelim","OptDelim"),("option","Option"),("optionList","OptionList"),("rule","Rule"),("ruleList","RuleList"),("strLit","StrLit"),("cl","Clause"),("r","Rule")]

quoteGrammarExp :: Data.Data a => String -> (Grammar -> a) -> String -> TH.ExpQ
quoteGrammarExp dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseGrammar $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToExpQ (const Nothing `Generics.extQ` antiGrammarExp `Generics.extQ` antiImportsOptExp `Generics.extQ` antiRuleExp `Generics.extQ` antiOptionExp `Generics.extQ` antiNameExp `Generics.extQ` antiClauseExp `Generics.extQ` antiOptDelimExp `Generics.extQ` antiStrLitExp) expr
quoteGrammarPat :: Data.Data a => String -> (Grammar -> a) -> String -> TH.PatQ
quoteGrammarPat dummy func s = do
  let s1 = replaceAllPatterns s
      expr = func $ parseGrammar $ alexScanTokens (dummy ++ " " ++ s1 ++ " " ++ dummy)
  dataToPatQ (const Nothing `Generics.extQ` antiGrammarPat `Generics.extQ` antiImportsOptPat `Generics.extQ` antiRulePat `Generics.extQ` antiOptionPat `Generics.extQ` antiNamePat `Generics.extQ` antiClausePat `Generics.extQ` antiOptDelimPat `Generics.extQ` antiStrLitPat) expr

antiStrLitExp :: StrLit -> Maybe (TH.Q TH.Exp )
antiStrLitExp ( Anti_StrLit v) = Just $ TH.varE (TH.mkName v)
antiStrLitExp _ = Nothing


antiOptDelimExp :: OptDelim -> Maybe (TH.Q TH.Exp )
antiOptDelimExp ( Anti_OptDelim v) = Just $ TH.varE (TH.mkName v)
antiOptDelimExp _ = Nothing


antiClauseExp :: Clause -> Maybe (TH.Q TH.Exp )
antiClauseExp ( Anti_Clause v) = Just $ TH.varE (TH.mkName v)
antiClauseExp _ = Nothing


antiNameExp :: [ Name ] -> Maybe (TH.Q TH.Exp)
antiNameExp ((Anti_Name v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiGrammarExp `Generics.extQ` antiImportsOptExp `Generics.extQ` antiRuleExp `Generics.extQ` antiOptionExp `Generics.extQ` antiNameExp `Generics.extQ` antiClauseExp `Generics.extQ` antiOptDelimExp `Generics.extQ` antiStrLitExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiNameExp _ = Nothing


antiOptionExp :: [ Option ] -> Maybe (TH.Q TH.Exp)
antiOptionExp ((Anti_Option v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiGrammarExp `Generics.extQ` antiImportsOptExp `Generics.extQ` antiRuleExp `Generics.extQ` antiOptionExp `Generics.extQ` antiNameExp `Generics.extQ` antiClauseExp `Generics.extQ` antiOptDelimExp `Generics.extQ` antiStrLitExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiOptionExp _ = Nothing


antiRuleExp :: [ Rule ] -> Maybe (TH.Q TH.Exp)
antiRuleExp ((Anti_Rule v):rest) =
 let restExp =   dataToExpQ (const Nothing `Generics.extQ` antiGrammarExp `Generics.extQ` antiImportsOptExp `Generics.extQ` antiRuleExp `Generics.extQ` antiOptionExp `Generics.extQ` antiNameExp `Generics.extQ` antiClauseExp `Generics.extQ` antiOptDelimExp `Generics.extQ` antiStrLitExp) rest
     lvar = TH.varE $ TH.mkName v
   in Just [| $lvar ++ $restExp |]
antiRuleExp _ = Nothing


antiImportsOptExp :: ImportsOpt -> Maybe (TH.Q TH.Exp )
antiImportsOptExp ( Anti_ImportsOpt v) = Just $ TH.varE (TH.mkName v)
antiImportsOptExp _ = Nothing


antiGrammarExp :: Grammar -> Maybe (TH.Q TH.Exp )
antiGrammarExp ( Anti_Grammar v) = Just $ TH.varE (TH.mkName v)
antiGrammarExp _ = Nothing



antiStrLitPat :: StrLit -> Maybe (TH.Q TH.Pat )
antiStrLitPat ( Anti_StrLit v) = Just $ TH.varP (TH.mkName v)
antiStrLitPat _ = Nothing


antiOptDelimPat :: OptDelim -> Maybe (TH.Q TH.Pat )
antiOptDelimPat ( Anti_OptDelim v) = Just $ TH.varP (TH.mkName v)
antiOptDelimPat _ = Nothing


antiClausePat :: Clause -> Maybe (TH.Q TH.Pat )
antiClausePat ( Anti_Clause v) = Just $ TH.varP (TH.mkName v)
antiClausePat _ = Nothing


antiNamePat :: [ Name ] -> Maybe (TH.Q TH.Pat)
antiNamePat [Anti_Name v] = Just $ TH.varP (TH.mkName v)
antiNamePat _ = Nothing


antiOptionPat :: [ Option ] -> Maybe (TH.Q TH.Pat)
antiOptionPat [Anti_Option v] = Just $ TH.varP (TH.mkName v)
antiOptionPat _ = Nothing


antiRulePat :: [ Rule ] -> Maybe (TH.Q TH.Pat)
antiRulePat [Anti_Rule v] = Just $ TH.varP (TH.mkName v)
antiRulePat _ = Nothing


antiImportsOptPat :: ImportsOpt -> Maybe (TH.Q TH.Pat )
antiImportsOptPat ( Anti_ImportsOpt v) = Just $ TH.varP (TH.mkName v)
antiImportsOptPat _ = Nothing


antiGrammarPat :: Grammar -> Maybe (TH.Q TH.Pat )
antiGrammarPat ( Anti_Grammar v) = Just $ TH.varP (TH.mkName v)
antiGrammarPat _ = Nothing



quoteGrammarType s = return TH.ListT
quoteGrammarDecs s = return []

getGrammar ( Ctr__Grammar__0 s) = s

grammar :: QuasiQuoter
grammar = QuasiQuoter (quoteGrammarExp "tok_Grammar_dummy_15" getGrammar ) (quoteGrammarPat "tok_Grammar_dummy_15" getGrammar ) quoteGrammarType quoteGrammarDecs

getClause ( Ctr__Grammar__1 s) = s

clause :: QuasiQuoter
clause = QuasiQuoter (quoteGrammarExp "tok_Clause_dummy_14" getClause ) (quoteGrammarPat "tok_Clause_dummy_14" getClause ) quoteGrammarType quoteGrammarDecs

getIdList ( Ctr__Grammar__2 s) = s

idList :: QuasiQuoter
idList = QuasiQuoter (quoteGrammarExp "tok_IdList_dummy_13" getIdList ) (quoteGrammarPat "tok_IdList_dummy_13" getIdList ) quoteGrammarType quoteGrammarDecs

getImportsOpt ( Ctr__Grammar__3 s) = s

importsOpt :: QuasiQuoter
importsOpt = QuasiQuoter (quoteGrammarExp "tok_ImportsOpt_dummy_12" getImportsOpt ) (quoteGrammarPat "tok_ImportsOpt_dummy_12" getImportsOpt ) quoteGrammarType quoteGrammarDecs

getName ( Ctr__Grammar__4 s) = s

name :: QuasiQuoter
name = QuasiQuoter (quoteGrammarExp "tok_Name_dummy_11" getName ) (quoteGrammarPat "tok_Name_dummy_11" getName ) quoteGrammarType quoteGrammarDecs

getOptDelim ( Ctr__Grammar__5 s) = s

optDelim :: QuasiQuoter
optDelim = QuasiQuoter (quoteGrammarExp "tok_OptDelim_dummy_10" getOptDelim ) (quoteGrammarPat "tok_OptDelim_dummy_10" getOptDelim ) quoteGrammarType quoteGrammarDecs

getOption ( Ctr__Grammar__6 s) = s

option :: QuasiQuoter
option = QuasiQuoter (quoteGrammarExp "tok_Option_dummy_9" getOption ) (quoteGrammarPat "tok_Option_dummy_9" getOption ) quoteGrammarType quoteGrammarDecs

getOptionList ( Ctr__Grammar__7 s) = s

optionList :: QuasiQuoter
optionList = QuasiQuoter (quoteGrammarExp "tok_OptionList_dummy_8" getOptionList ) (quoteGrammarPat "tok_OptionList_dummy_8" getOptionList ) quoteGrammarType quoteGrammarDecs

getRule ( Ctr__Grammar__8 s) = s

rule :: QuasiQuoter
rule = QuasiQuoter (quoteGrammarExp "tok_Rule_dummy_7" getRule ) (quoteGrammarPat "tok_Rule_dummy_7" getRule ) quoteGrammarType quoteGrammarDecs

getRuleList ( Ctr__Grammar__9 s) = s

ruleList :: QuasiQuoter
ruleList = QuasiQuoter (quoteGrammarExp "tok_RuleList_dummy_6" getRuleList ) (quoteGrammarPat "tok_RuleList_dummy_6" getRuleList ) quoteGrammarType quoteGrammarDecs

getStrLit ( Ctr__Grammar__10 s) = s

strLit :: QuasiQuoter
strLit = QuasiQuoter (quoteGrammarExp "tok_StrLit_dummy_5" getStrLit ) (quoteGrammarPat "tok_StrLit_dummy_5" getStrLit ) quoteGrammarType quoteGrammarDecs

