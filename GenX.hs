module GenX (genX)
    where

import Parser
import Text.PrettyPrint
import Grammar

genX :: NormalGrammar -> String
genX g@(NormalGrammar name _ lex_rules) = 
    render $ vcat [
                   header,
                   nl,
                   tokens,
                   nl,
                   footer
                  ]
    where tokens = genTokens lex_rules
          adt = genTokenADT lex_rules
          header = vcat [text "{", text "module" <+> text name <> text "Lexer", text "where }", 
                         text "%wrapper \"basic\""]
          footer = vcat [text "{", adt, text "}"]
          nl = text ""

genTokenADT :: [LexicalRule] -> Doc
genTokenADT lexical_rules = text "data" <+> text "Token" <+> text "=" <+> joinAlts (map makeToken lexical_rules)
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
                   "Keyword" -> text "{ \\_ ->" <+> token_name <+> text "}"
                   "Ignore"  -> text ";"
                   _         -> text "{ \\__s ->" <+> token_name <+> (parens $ text func <+> text "__s") <+> text "}"

backquoteStr :: String -> String
backquoteStr str = concat (map (\chr -> if (case chr of
                                                 '+' -> True
                                                 '*' -> True
                                                 '"' -> True
                                                 '[' -> True
                                                 ']' -> True
                                                 '(' -> True
                                                 ')' -> True
                                                 _   -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

translateClause (IId _)             = text "<TODO: macro ref here>"
translateClause (IStrLit s)         = doubleQuotes $ text $ backquoteStr s
translateClause (IDot)              = text "."
translateClause (IRegExpLit re)     = brackets $ text $ backquoteStr re
translateClause (IStar cl Nothing)  = translateClause cl <+> text "*"
-- a* ~x --> (a(x a)*)?
translateClause (IStar cl (Just _)) = error $ "Star (*) clauses with delimiters are not supported in lexical rules"
translateClause (IPlus cl Nothing)  = translateClause cl <+> text "+"
translateClause (IPlus cl (Just _)) = error $ "Plus (+) clauses with delimiters are not supported in lexical rules"
translateClause (IAlt clauses)      = hsep $ punctuate (text "|") (map translateClause clauses)
translateClause (ISeq clauses)    = hsep $ punctuate (text " ") (map translateClause clauses)
translateClause (IOpt clause)       = translateClause clause <+> text "?"
translateClause cl                 = error $ "Can't translate to lexer spec: " ++ (show cl)

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)
