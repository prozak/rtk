{-# LANGUAGE QuasiQuotes #-}
module GenX (genX)
    where

import Parser
import Text.PrettyPrint
import Grammar
import StrQuote

genX :: NormalGrammar -> String
genX g@(NormalGrammar { getNGrammarName = name, getLexicalRules = lex_rules, getNImports = imports}) = 
    render $ vcat [
                   header,
                   nl,
                   tokens,
                   nl,
                   footer
                  ]
    where tokens = genTokens lex_rules
          adt = genTokenADT lex_rules
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
genTokens lexical_rules = text "tokens" <+> text ":-" <+> vcat (map makeToken lexical_rules)
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
                                                        _   -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

translateClause (IId _)             = text "<TODO: macro ref here>"
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
