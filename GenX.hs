module GenX (genX)
    where

import Parser
import Text.PrettyPrint
import Grammar

genX :: NormalGrammar -> String
genX g@(Grammar name rules) = render $ vcat [
                                             header,
                                             nl,
                                             tokens,
                                             nl,
                                             footer
                                            ]
    where lex_rules = lexicalRules g
          tokens = genTokens lex_rules
          adt = genTokenADT lex_rules
          header = vcat [text "{", text "module" <+> text name <> text "Lexer", text "where }", 
                         text "%wrapper \"basic\""]
          footer = vcat [text "{", adt, text "}"]
          nl = text ""

genTokenADT :: [NormalRule] -> Doc
genTokenADT lexical_rules = text "data" <+> text "Token" <+> text "=" <+> joinAlts (map makeToken lexical_rules)
    where makeToken Rule { getDataTypeName = data_type, getRuleName = name } =
            let token_name = text $ tokenName name in
              case data_type of
                   "Keyword" -> token_name
                   "Ignore"  -> empty
                   _         -> token_name <+> text data_type

genTokens :: [NormalRule] -> Doc
genTokens lexical_rules = text "tokens" <+> text ":-" <+> vcat (map makeToken lexical_rules)
    where makeToken Rule { getDataTypeName = data_type, getDataFunc = func, getRuleName = name, getClause = cl } =
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

translateClause (Id _)             = text "<TODO: macro ref here>"
translateClause (StrLit s)         = doubleQuotes $ text $ backquoteStr s
translateClause (Dot)              = text "."
translateClause (RegExpLit re)     = brackets $ text $ backquoteStr re
translateClause (Star cl Nothing)  = translateClause cl <+> text "*"
-- a* ~x --> (a(x a)*)?
translateClause (Star cl (Just _)) = error $ "Star (*) clauses with delimiters are not supported in lexical rules"
translateClause (Plus cl Nothing)  = translateClause cl <+> text "+"
translateClause (Plus cl (Just _)) = error $ "Plus (+) clauses with delimiters are not supported in lexical rules"
translateClause (Alt clauses)      = hsep $ punctuate (text "|") (map translateClause clauses)
translateClause (Seq _ clauses)    = hsep $ punctuate (text " ") (map translateClause clauses)
translateClause (Opt clause)       = translateClause clause <+> text "?"
translateClause cl                 = error $ "Can't translate to lexer spec: " ++ (show cl)

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)
