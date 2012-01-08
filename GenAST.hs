module GenAST (genAST)
    where

import Parser
import Text.PrettyPrint
import Grammar
import qualified Data.Map as Map

genAST :: NormalGrammar -> String
genAST grammar = render $ vcat (map (genRule rules_map) (normalRules grammar))
    where rules_map = rulesMap grammar

genRule :: RulesMap -> NormalRule -> Doc
genRule rmap Rule { getDataTypeName = type_name, getClause = clause } =
    case clause of
         (Seq _ s@[(Star _ _)]) -> genType rmap type_name s
         (Seq _ s@[(Plus _ _)]) -> genType rmap type_name s
         (Seq _ s@[(Opt _)])    -> genType rmap type_name s
         s@(Seq _ _)            -> genData rmap type_name [s]
         (Alt sequences)        -> genData rmap type_name sequences

genType :: RulesMap -> String -> [Clause] -> Doc
genType rmap name clauses = text "type" <+> text name <+> text "=" <+> (hsep $ map (genItem rmap) clauses)

genData :: RulesMap -> String -> [Clause] -> Doc
genData rmap name sequences = text "data" <+> text name <+> text "=" <+> joinAlts (map (genConstructor rmap) sequences)

genConstructor :: RulesMap -> Clause -> Doc
genConstructor rmap (Seq constructor clauses) = text constructor <+> (hsep $ map (genItem rmap) clauses)

genItem :: RulesMap -> Clause -> Doc

genItem rmap (Star cl _) = brackets $ genItem rmap cl
genItem rmap (Plus cl _) = brackets $ genItem rmap cl
genItem rmap (Opt cl) = parens $ text "Maybe" <+> genItem rmap cl

genItem rmap (Id id) = text $ getDataTypeName $ findRule rmap id
genItem _    (Ignore cl) = empty
genItem _    (Lifted id) = error "lifted rules are not yet implemented"
genItem _    cl = error $ "Can't generate AST for simple clause: " ++ (show cl)

findRule :: RulesMap -> String -> NormalRule
findRule rmap id = case Map.lookup id rmap of
                        Just r -> r
                        _      -> error $ "Reference to unknown rule " ++ id

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts
