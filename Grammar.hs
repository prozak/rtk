module Grammar where

import Syntax

import qualified GrammarParser as GP

-- | The verbatim source text of a fixed-literal lexical rule - a keyword or
-- punctuation token. String literals normalize to such rules, whose clause is
-- a bare 'GP.Lit', and that literal's content is the token's only spelling.
-- This is the one place terminal text is reconstructed from a lexical clause;
-- both the generated parser's error messages ('GenY.genShowToken') and the
-- generated pretty-printer ('GenPP') reuse it. A rule whose clause is not a
-- bare literal has no single spelling, so 'Nothing' is returned and callers
-- fall back to the rule name.
literalTokenText :: LClause -> Maybe String
literalTokenText (GP.Lit _ (GP.Str _ s)) = Just s
literalTokenText _                       = Nothing

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
