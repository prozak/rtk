module Grammar where

import Parser

normalRules :: [SyntaxRuleGroup] -> [SyntaxRule]
normalRules groups = concat $ map getSRules groups

tokenName :: String -> String
tokenName name = "Tk__" ++ name

isClauseSeqLifted :: [SyntaxSimpleClause] -> Bool
isClauseSeqLifted sseq = case filter isNotIgnored sseq of
                           [SSLifted{}] -> True
                           _ -> if any isLifted sseq
                                  then error $ "Lifted rule cannot be in the sequence" ++ show  sseq
                                  else False
    where isLifted SSLifted{} = True
          isLifted _          = False
isNotIgnored :: SyntaxSimpleClause -> Bool
isNotIgnored SSId{} = True
isNotIgnored SSLifted{} = True
isNotIgnored _ = False

isSymmacro :: LexicalRule -> Bool
isSymmacro (MacroRule _ _) = True
isSymmacro _               = False

removeSymmacros :: [LexicalRule] -> [LexicalRule]
removeSymmacros lst =  filter (not . isSymmacro) lst
