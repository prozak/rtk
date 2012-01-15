module GenAST (genAST)
    where

import Parser
import Text.PrettyPrint
import Grammar
import qualified Data.Map as Map

normalRulesNamed :: [SyntaxRuleGroup] -> [(ID, SyntaxTopClause)]
normalRulesNamed groups = map (\g -> (getSDataTypeName g, combineClauses $ map getSClause $ getSRules g))
                          groups

combineClauses :: [SyntaxTopClause] -> SyntaxTopClause
combineClauses [a] = a
combineClauses alts = STAltOfSeq $ concat $ map (\(STAltOfSeq seqs) -> seqs) alts

type RulesMap = Map.Map ID ID

rulesMap :: NormalGrammar -> RulesMap
rulesMap NormalGrammar{ getSyntaxRuleGroups = groups, getLexicalRules = lrules } = 
    Map.fromList $ concat 
            (map (\ lr -> (getLRuleName lr, getLRuleDataType lr)) lrules : 
             map (\ g -> map (\r -> (getSRuleName r, getSDataTypeName g)) $ getSRules g) groups)

genAST :: NormalGrammar -> String
genAST grammar = render $ vcat (map (genRule rules_map) (normalRulesNamed $ getSyntaxRuleGroups grammar))
    where rules_map = rulesMap grammar

genRule :: RulesMap -> (ID, SyntaxTopClause) -> Doc
genRule rmap (type_name, clause) =
    case clause of
         s@(STMany _ _ _) -> genType rmap type_name [s]
         s@(STOpt _)      -> genType rmap type_name [s]
         (STAltOfSeq sequences)        -> genData rmap type_name sequences

genType :: RulesMap -> String -> [SyntaxTopClause] -> Doc
genType rmap name clauses = text "type" <+> text name <+> text "=" <+> (hsep $ map (genItem rmap) clauses)

needGenereateAlt :: STSeq -> Bool
needGenereateAlt (STSeq _ seqs) = not $ isClauseSeqLifted seqs

genData :: RulesMap -> String -> [STSeq] -> Doc
genData rmap name sequences = text "data" <+> text name <+> text "=" <+> (joinAlts (map (genConstructor rmap) sequences') 
                                                                          $$ text "deriving (Ord, Eq, Show)")
    where sequences' = filter needGenereateAlt sequences

genConstructor :: RulesMap -> STSeq -> Doc
genConstructor rmap (STSeq constructor clauses) = text constructor <+> (hsep $ map (genSimpleItem rmap) clauses)

genItem :: RulesMap -> SyntaxTopClause -> Doc

genItem rmap (STMany _ cl _) = brackets $ genSimpleItem rmap cl
genItem rmap (STOpt cl) = parens $ text "Maybe" <+> genSimpleItem rmap cl

genSimpleItem :: RulesMap -> SyntaxSimpleClause -> Doc
genSimpleItem rmap (SSId id) = text $ findRuleDataTypeName rmap id
genSimpleItem _    (SSIgnore id) = empty
genSimpleItem _    (SSLifted id) = error "lifted rules are not yet implemented"

findRuleDataTypeName :: RulesMap -> ID -> ID
findRuleDataTypeName rmap id = case Map.lookup id rmap of
                                 Just r -> r
                                 _      -> error $ "Reference to unknown rule " ++ id

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts
