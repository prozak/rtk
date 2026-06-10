module GenAST (genAST)
    where

import Parser
import Diagnostics (Diagnostic(..))
import Text.PrettyPrint
import Grammar
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe (mapMaybe)

normalRulesNamed :: [SyntaxRuleGroup] -> [(ID, SyntaxTopClause)]
normalRulesNamed groups = map (\g -> (getSDataTypeName g, combineClauses $ map getSClause $ getSRules g))
                          groups

combineClauses :: [SyntaxTopClause] -> SyntaxTopClause
combineClauses [a] = a
combineClauses alts = STAltOfSeq $ deduplicateByConstructor $ concat $ map extractSeqs alts
  where extractSeqs (STAltOfSeq seqs) = seqs
        extractSeqs _ = []
        -- Deduplicate alternatives with the same constructor name (e.g., Anti_Expression)
        -- This is necessary for shared types where the same anti-alternative is added to multiple rules
        deduplicateByConstructor seqs = List.nubBy sameConstructor seqs
        sameConstructor (STSeq c1 _) (STSeq c2 _) = c1 == c2

type RulesMap = Map.Map ID ID

rulesMap :: NormalGrammar -> RulesMap
rulesMap NormalGrammar{ getSyntaxRuleGroups = groups, getLexicalRules = lrules } = 
    Map.fromList $ concat 
            (mapMaybe lexRuleEntry lrules :
             map (\ g -> map (\r -> (getSRuleName r, getSDataTypeName g)) $ getSRules g) groups)

-- Macro rules are inlined into the lexer spec and carry no data type, so
-- they contribute nothing to the rules map.
lexRuleEntry :: LexicalRule -> Maybe (ID, ID)
lexRuleEntry LexicalRule{ getLRuleName = name, getLRuleDataType = dt } = Just (name, dt)
lexRuleEntry MacroRule{} = Nothing

genAST :: NormalGrammar -> Either Diagnostic String
genAST grammar = do
    docs <- mapM (genRule rules_map) (normalRulesNamed $ getSyntaxRuleGroups grammar)
    return $ render $ vcat docs
  where rules_map = rulesMap grammar

genRule :: RulesMap -> (ID, SyntaxTopClause) -> Either Diagnostic Doc
genRule rmap (type_name, clause) =
    case clause of
         s@(STMany _ _ _) -> genType rmap type_name [s]
         s@(STOpt _)      -> genType rmap type_name [s]
         (STAltOfSeq sequences)        -> genData rmap type_name sequences

genType :: RulesMap -> String -> [SyntaxTopClause] -> Either Diagnostic Doc
genType rmap name clauses = do
    items <- mapM (genItem rmap name) clauses
    return $ text "type" <+> text name <+> text "=" <+> hsep items

needGenereateAlt :: STSeq -> Bool
needGenereateAlt (STSeq _ seqs) = not $ isClauseSeqLifted seqs

genData :: RulesMap -> String -> [STSeq] -> Either Diagnostic Doc
genData rmap name sequences = do
    ctors <- mapM (genConstructor rmap name) sequences'
    return $ text "data" <+> text name <+> text "=" <+> (joinAlts ctors
                                                         $$ text "deriving (Ord, Eq, Show, Gen.Data, Gen.Typeable)")
    where sequences' = filter needGenereateAlt sequences

genConstructor :: RulesMap -> String -> STSeq -> Either Diagnostic Doc
genConstructor rmap refType (STSeq constructor clauses) = do
    items <- mapM (genSimpleItem rmap refType) clauses
    return $ text constructor <+> hsep items

genItem :: RulesMap -> String -> SyntaxTopClause -> Either Diagnostic Doc
genItem rmap refType (STMany _ cl _) = brackets <$> genSimpleItem rmap refType cl
genItem rmap refType (STOpt cl) = (\d -> parens (text "Maybe" <+> d)) <$> genSimpleItem rmap refType cl
genItem _ _ (STAltOfSeq _) = error "STAltOfSeq not supported in genItem"

genSimpleItem :: RulesMap -> String -> SyntaxSimpleClause -> Either Diagnostic Doc
genSimpleItem rmap refType (SSId idName) = text <$> findRuleDataTypeName rmap refType idName
genSimpleItem _    _       (SSIgnore _) = Right empty
genSimpleItem _    _       (SSLifted _) = error "lifted rules are not yet implemented"

-- A reference to an unknown rule is a user error (a typo'd rule name); name
-- both the unknown rule and the type that references it.
findRuleDataTypeName :: RulesMap -> String -> ID -> Either Diagnostic ID
findRuleDataTypeName rmap refType idName = case Map.lookup idName rmap of
                                 Just r -> Right r
                                 _      -> Left $ Diagnostic Nothing (Just ("in type '" ++ refType ++ "'"))
                                                  ("reference to unknown rule '" ++ idName ++ "'")

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") alts
