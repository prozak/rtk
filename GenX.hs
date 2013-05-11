{-# LANGUAGE QuasiQuotes #-}
module GenX (genX)
    where

import Parser
import Text.PrettyPrint
import qualified Data.Set as S
import Grammar
import StrQuote

getMacroIdsFromClause :: S.Set String -> IClause -> S.Set String
getMacroIdsFromClause result (IId str) = S.insert str result
getMacroIdsFromClause result (IOpt clause) = getMacroIdsFromClause result clause
getMacroIdsFromClause result (IPlus clause _) = getMacroIdsFromClause result clause
getMacroIdsFromClause result (IStar clause _) = getMacroIdsFromClause result clause
getMacroIdsFromClause result (ISeq clauses) = S.union result (S.unions $ map (getMacroIdsFromClause result) clauses)
getMacroIdsFromClause result (IAlt clauses) = S.union result (S.unions $ map (getMacroIdsFromClause result) clauses)
getMacroIdsFromClause result _ = result

getMacroIdsHelper :: LexicalRule -> S.Set String
getMacroIdsHelper LexicalRule{ getLClause = cl } = getMacroIdsFromClause S.empty cl

getMacroIds :: [LexicalRule] -> S.Set String
getMacroIds lexRules = foldr (\lexRule result -> S.union result $ getMacroIdsHelper lexRule) S.empty lexRules

genMacroText :: S.Set String -> [LexicalRule] -> Doc
genMacroText macroIds tokens =
    vcat $ foldl (\result (LexicalRule {getLRuleName = name, getLClause = cl }) -> 
                        if name `S.member` macroIds
                          then text "@" <> text name <+> text "=" <+> translateClause cl : result
                          else result)
             [] tokens

genX :: NormalGrammar -> String
genX g@(NormalGrammar { getNGrammarName = name, getLexicalRules = tokens, getNImports = imports}) = 
    render $ vcat [
                   header,
                   nl,
                   macroText,
                   nl,
                   tokensText,
                   nl,
                   footer
                  ]
    where macroIds = getMacroIds tokens
          tokensText = genTokens tokens
          macroText = genMacroText macroIds tokens
          adt = genTokenADT tokens
          header = vcat [text "{", text "module" <+> text name <> text "Lexer(alexScanTokens, Token(..))", text "where", text imports, text " }", 
                         text "%wrapper \"monad\""]
          funs_text = [str|
alexEOF = return EndOfFile
alexScanTokens :: String -> [Token]
alexScanTokens str = 
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> error err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       EndOfFile -> return $ reverse toks
                       _ -> let toks' = tok : toks 
                            in toks' `seq` loop toks'
  loop []
simple1 :: (String -> Token) -> AlexInput -> Int -> Alex Token
simple1 t (_, _, _, str) len = return $ t (take len str)

simple t input len = return t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column" ++ ". Following chars :" ++ (take 10 str)
|]
          funs = text funs_text             
          footer = vcat [text "{", adt, funs , text "}"]
          nl = text ""

genTokenADT :: [LexicalRule] -> Doc
genTokenADT lexical_rules = text "data" <+> text "Token" <+> text "=" <+> (joinAlts (text "EndOfFile" : (map makeToken lexical_rules)) $$ text "deriving (Show)")
    where makeToken LexicalRule { getLRuleDataType = data_type, getLRuleName = name } =
            let token_name = text $ tokenName name in
              case data_type of
                   "Keyword" -> token_name
                   "Ignore"  -> empty
                   _         -> token_name <+> text data_type

genTokens :: [LexicalRule] -> Doc
genTokens lexical_rules = text "tokens" <+> text ":-" <+> vcat (map makeToken lexical_rules) <+> vcat [text ". { rtkError }"]
    where makeToken LexicalRule { getLRuleDataType = data_type, getLRuleFunc = func, getLRuleName = name, getLClause = cl } =
              translateClause cl <+> makeProduction name data_type func
          makeProduction name data_type func =
            let token_name = text $ tokenName name in
              case data_type of
                   "Keyword" -> text "{ simple" <+> token_name <+> text "}"
                   "Ignore"  -> text ";"
                   _         -> text "{ simple1 $ " <+> token_name <+> text "." <+> (parens $ text func) <+> text "}"

backquoteStr :: String -> String
backquoteStr str = concat (map (\chr -> if (case chr of
                                                 '"'  -> True
                                                 _    -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

backquoteStrInBrackets :: String -> String
backquoteStrInBrackets str = concat (map (\chr -> if (case chr of
                                                        '[' -> True
                                                        ']' -> True
                                                        ' ' -> True
                                                        '*' -> True
                                                        '/' -> True
                                                        '{' -> True
                                                        '}' -> True
                                                        '$' -> True
                                                        _   -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

translateClause (IId name)          = text "@" <> text name
translateClause (IStrLit s)         = doubleQuotes $ text $ backquoteStr s
translateClause (IDot)              = text "."
translateClause (IRegExpLit re)     = brackets $ text $ backquoteStrInBrackets re
translateClause (IStar cl Nothing)  = translateClause cl <+> text "*"
-- a* ~x --> (a(x a)*)?
translateClause (IStar cl (Just _)) = error $ "Star (*) clauses with delimiters are not supported in lexical rules"
translateClause (IPlus cl Nothing)  = translateClause cl <+> text "+"
translateClause (IPlus cl (Just _)) = error $ "Plus (+) clauses with delimiters are not supported in lexical rules"
translateClause (IAlt clauses)      = parens $ hsep $ punctuate (text "|") (map translateClause clauses)
translateClause (ISeq clauses)    = hsep $ punctuate (text " ") (map translateClause clauses)
translateClause (IOpt clause)       = translateClause clause <+> text "?"
translateClause cl                 = error $ "Can't translate to lexer spec: " ++ (show cl)

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)
