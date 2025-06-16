module GenY (genY)
    where

import Parser
import Text.PrettyPrint hiding ((<>))
import qualified Data.Set as Set
import GenAST
import Grammar

genY :: NormalGrammar -> String
genY g@(NormalGrammar name srules lex_rules _ _ _ _) = 
    render $ vcat [
                   text header,
                   nl,
                   text "%name parse" <> text name,
                   text "%tokentype { L.Token }",
                   text "%error { \\ rest -> error $ \"Parse error \" ++ (show rest) }",
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
          listRuleSet = makeListRuleSet normal_rules
          rulesDoc = vcat (map (genRule listRuleSet) normal_rules)
          lexDoc = vcat (map genToken $ removeSymmacros lex_rules)
          nl = text ""
          header = "{\n\
                   \{-# LANGUAGE DeriveDataTypeable #-}\n\
                   \module " ++ name ++ "Parser where\n\
                   \import qualified Data.Generics as Gen\n\
                   \import qualified " ++ name ++  "Lexer as L (Token(..), alexScanTokens)\n\
                   \}"
          ast = genAST g
          footer = "{\n" ++ ast ++ "\n}"

type ListRuleSet = Set.Set ID

makeListRuleSet :: [SyntaxRule] -> ListRuleSet
makeListRuleSet lst = Set.fromList $ map getSRuleName $ filter isMany lst
    where isMany (SyntaxRule { getSClause = STMany{} }) = True
          isMany _ = False

genToken :: LexicalRule -> Doc
genToken LexicalRule{ getLRuleName = name, getLRuleDataType = dtn } =
    case dtn of
        "Keyword" -> combineAlt (text name) (text "L." <> text (tokenName name))
        "Ignore"  -> empty
        _         -> combineAlt (text name) ((text "L." <> text (tokenName name)) <+> text "$$")

genRule :: ListRuleSet -> SyntaxRule -> Doc
-- <Rule>* with separator can only be expressed using two rules in LR grammar
-- Will do it here 
genRule listRuleSet SyntaxRule{ getSClause = (STMany STStar cl (Just cl1)), getSRuleName = name } = 
    let lstName = name ++ "__plus_list_"
    in
      vcat [
            ((text lstName) <+> (text ":") <+> (genTopClause listRuleSet lstName 
                                               (STMany STPlus cl (Just cl1)))) <> text "\n",
            ((text name) <+> (text ":") <+> (genOptPlusClause lstName)) <> text "\n"
           ]
          

genRule listRuleSet SyntaxRule{ getSClause = cl, getSRuleName = name } = 
    ((text name) <+> (text ":") <+> (genTopClause listRuleSet name cl)) <> text "\n"

genTopClause :: ListRuleSet -> String -> SyntaxTopClause -> Doc

-- Ignore is not expected in the cl
genTopClause lrs rn (STMany op cl Nothing) = joinAlts [base, step]
    where (baseAlt,alt) = case op of
                            STStar -> ("[]", emptyAlt)
                            STPlus -> ("[$1]", clDoc)
          base = combineAlt alt (text baseAlt)
          step = combineAlt (text rn <+> clDoc) (text "$2 : $1")
          clDoc = genSimpleClause cl

genTopClause lrs rn (STMany STPlus cl (Just cl1)) = joinAlts [base, step]
    where base = combineAlt clDoc (text "[$1]")
          step = combineAlt (text rn <+> genSimpleClause cl1 <+> clDoc) (text "$3 : $1")
          clDoc = genSimpleClause cl

genTopClause lrs _ (STOpt cl) = joinAlts [present, not_present]
    where present = combineAlt (genSimpleClause cl)
                               (text "Just" <+> constructor_call)
          constructor_call = hsep $ enumClauses lrs [cl]
          not_present = combineAlt emptyAlt (text "Nothing")

genTopClause lrs rn (STAltOfSeq clauses) = joinAlts $ map (genClauseSeq lrs) clauses

genOptPlusClause lstName = joinAlts [present, not_present]
    where present = combineAlt (text lstName)
                               (text "$1")
          not_present = combineAlt emptyAlt (text "[]")

genClauseSeq :: ListRuleSet -> STSeq -> Doc
genClauseSeq lrs (STSeq constructor clauses) | isClauseSeqLifted clauses = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (enumClauses lrs clauses)
genClauseSeq lrs (STSeq constructor clauses)  = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (text constructor) : (enumClauses lrs clauses)

genSimpleClause :: SyntaxSimpleClause -> Doc
-- TODO: check whether reverse is needed (monad again) (switch to left recursion)
genSimpleClause (SSId id) = text id
genSimpleClause (SSIgnore id) = text id
 -- TODO: no lifted yet, need monad with rules map here
genSimpleClause (SSLifted id) = text id

enumClauses :: ListRuleSet -> [SyntaxSimpleClause] -> [Doc]
enumClauses lrs cls = f cls 1 []
    where f (ssc:tail) count acc | isNotIgnored ssc = f tail (count + 1) 
                                                        (if Set.member (getSID ssc) lrs
                                                            then ((text "(reverse $" <> int count <> text ")") : acc) 
                                                            else ((text "$" <> int count) : acc))
          f (_:       tail) count acc               = f tail (count + 1) acc
          f []              _     acc               = reverse acc
          getSID (SSId n) = n
          getSID (SSLifted n) = n

emptyAlt = text "{- empty -}"

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts

combineAlt :: Doc -> Doc -> Doc
combineAlt rule production = rule <+> text "{" <+> production <+> text "}"

