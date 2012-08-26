module GenY (genY)
    where

import Parser
import Text.PrettyPrint
import qualified Data.Map as Map
import GenAST
import Grammar

genY :: NormalGrammar -> String
genY g@(NormalGrammar name srules lex_rules info) = 
    render $ vcat [
                   text header,
                   nl,
                   text "%name parse" <> text name,
                   text "%tokentype { L.Token }",
                   text "%error { \\ rest -> error $ \"Parse error\" ++ (show rest) }",
                   nl,
                   text "%token",
                   nl,
                   lexDoc,
                   nl,
                   text "%%",
                   nl,
                   rulesDoc,
                   nl,
                   text footer
                  ]
    where normal_rules = normalRules srules
          rulesDoc = vcat (map genRule normal_rules)
          lexDoc = vcat (map genToken lex_rules)
          nl = text ""
          header = "{\n\
		   \{-# LANGUAGE DeriveDataTypeable #-}\n\
                   \module " ++ name ++ "Parser where\n\
		   \import Data.Generics\n\
                   \import qualified " ++ name ++  "Lexer as L (Token(..), alexScanTokens)\n\
                   \}"
          ast = genAST g
          footer = "{\n" ++ ast ++ "\n}"

genToken :: LexicalRule -> Doc
genToken LexicalRule{ getLRuleName = name, getLRuleDataType = dtn } =
    case dtn of
        "Keyword" -> combineAlt (text name) (text "L." <> text (tokenName name))
        "Ignore"  -> empty
        _         -> combineAlt (text name) (text "L." <> text (tokenName name) <+> text "$$")

genRule :: SyntaxRule -> Doc
genRule SyntaxRule{ getSClause = cl, getSRuleName = name } = (text name) <+> (text ":") <+> (genTopClause name cl) <> text "\n"

genTopClause :: String -> SyntaxTopClause -> Doc

-- Ignore is not expected in the cl
genTopClause rn (STMany op cl Nothing) = joinAlts [base, step]
    where (baseAlt,alt) = case op of
                            STStar -> ("[]", emptyAlt)
                            STPlus -> ("[$1]", clDoc)
          base = combineAlt alt (text baseAlt)
          step = combineAlt (clDoc <+> text rn) (text "$1 : $2")
          clDoc = genSimpleClause cl

genTopClause rn (STMany op cl (Just cl1)) = joinAlts [base, step]
    where (baseAlt,alt) = case op of
                            STStar -> ("[]", emptyAlt)
                            STPlus -> ("[$1]", clDoc)
          base = combineAlt alt (text baseAlt)
          step = combineAlt (clDoc <+> genSimpleClause cl1 <+> text rn) (text "$1 : $3")
          clDoc = genSimpleClause cl

genTopClause _ (STOpt cl) = joinAlts [present, not_present]
    where present = combineAlt (genSimpleClause cl)
                               (text "Just" <+> constructor_call)
          constructor_call = hsep $ enumClauses [cl]
          not_present = combineAlt emptyAlt (text "Nothing")

genTopClause rn (STAltOfSeq clauses) = joinAlts $ map genClauseSeq clauses

genClauseSeq :: STSeq -> Doc
genClauseSeq (STSeq constructor clauses) | isClauseSeqLifted clauses = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (enumClauses clauses)
genClauseSeq (STSeq constructor clauses)  = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (text constructor) : (enumClauses clauses)

genSimpleClause :: SyntaxSimpleClause -> Doc
-- TODO: check whether reverse is needed (monad again) (switch to left recursion)
genSimpleClause (SSId id) = text id
genSimpleClause (SSIgnore id) = text id
 -- TODO: no lifted yet, need monad with rules map here
genSimpleClause (SSLifted id) = text id

enumClauses :: [SyntaxSimpleClause] -> [Doc]
enumClauses cls = f cls 1 []
    where f (ssc:tail) count acc | isNotIgnored ssc = f tail (count + 1) ((text "$" <> int count) : acc)
          f (_:       tail) count acc               = f tail (count + 1) acc
          f []              _     acc               = reverse acc

emptyAlt = text "{- empty -}"

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts

combineAlt :: Doc -> Doc -> Doc
combineAlt rule production = rule <+> text "{" <+> production <+> text "}"

