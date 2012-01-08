module GenY (genY)
    where

import Parser
import Text.PrettyPrint.HughesPJ
import qualified Data.Map as Map
import Text.Printf

rulesMap :: NormalGrammar -> Map.Map String NormalRule
rulesMap Grammar{ getRules = rules } = Map.fromList $ map (\ r -> (getRuleName r, r)) rules

genY :: NormalGrammar -> String
genY (Grammar name rules) = render $ vcat [
                                             text header,
                                             nl,
                                             text "%name parse" <> text name,
                                             text "%tokentype L.Token",
                                             nl,
                                             text "%token",
                                             nl,
                                             lexDoc,
                                             nl,
                                             text "%%",
                                             nl,
                                             rulesDoc
                                          ]
    where normal_rules = filter (not.isLexicalRule.getRuleName) rules
          lex_rules = filter (isLexicalRule.getRuleName) rules
          rulesDoc = vcat (map genRule normal_rules)
          lexDoc = vcat (map genToken lex_rules)
          nl = text ""
          header = "{\n\
                   \module " ++ name ++ "Parser\n\
                   \import qualified Lexer as L (Token(..), alexScanTokens)\n\
                   \}"

genToken :: NormalRule -> Doc
genToken Rule{ getRuleName = name } = combineAlt (text name) (text "L.Tk__" <> text name)

genRule :: NormalRule -> Doc
genRule Rule{ getClause = cl, getRuleName = name } = (text name) <+> (text ":") <+> (genClause name cl) <> text "\n"

genClause :: String -> Clause -> Doc

-- Ignore is not expected in the cl
genClause rn (Seq constructor [(Star cl Nothing)]) = joinAlts [base, step]
    where base = combineAlt emptyAlt (text "[]")
          step = combineAlt ((genSimpleClause cl) <+> text rn) (text "$1 : $2")

genClause rn (Seq constructor [(Star cl (Just cl1))]) = joinAlts [base, step]
    where base = combineAlt emptyAlt (text "[]")
          step = combineAlt (genSimpleClause cl <+> genSimpleClause cl1 <+> text rn) (text "$1 : $3")

genClause rn (Seq constructor [(Plus cl Nothing)]) = joinAlts [base, step]
    where base = combineAlt clDoc (text "[ $1 ]")
          step = combineAlt (clDoc <+> text rn) (text "$1 : $2")
          clDoc = genSimpleClause cl

genClause rn (Seq constructor [(Plus cl (Just cl1))]) = joinAlts [base, step]
    where base = combineAlt clDoc (text "[ $1 ]")
          step = combineAlt (clDoc <+> genSimpleClause cl1 <+> text rn) (text "$1 : $2")
          clDoc = genSimpleClause cl

genClause _ (Seq constructor [(Opt cl)]) = joinAlts [present, not_present]
    where present = combineAlt (genSimpleClause cl)
                               (text "Just" <+> parens constructor_call)
          constructor_call = hsep $ (text constructor) : enumClauses [cl]
          not_present = combineAlt emptyAlt (text "Nothing")

genClause _ (Seq constructor clauses) = combineAlt rule production
    where rule = hsep (map genSimpleClause clauses)
          production = hsep $ (text constructor) : (enumClauses clauses)

genClause rn (Alt clauses) = joinAlts $ map (genClause rn) clauses

genClause _ cl = error $ "Can't generate Y for clause: " ++ (show cl)

genSimpleClause :: Clause -> Doc
-- TODO: check whether reverse is needed (monad again) (switch to left recursion)
genSimpleClause (Id id) = text id
genSimpleClause (Ignore cl) = genSimpleClause cl
 -- TODO: no lifted yet, need monad with rules map here
genSimpleClause (Lifted id) = error "lifted rules are not yet implemented"
genSimpleClause cl = error $ "Can't generate Y for simple clause: " ++ (show cl)

enumClauses :: [Clause] -> [Doc]
enumClauses cls = f cls 1 []
    where f ((Id _):tail) count acc = f tail (count + 1) ((text "$" <> int count) : acc)
          f (_:     tail) count acc = f tail (count + 1) acc
          f []            _     acc = reverse acc

emptyAlt = text "{- empty -}"

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts

combineAlt :: Doc -> Doc -> Doc
combineAlt rule production = rule <+> text "{" <+> production <+> text "}"

