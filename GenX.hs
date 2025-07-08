{-# LANGUAGE QuasiQuotes #-}
module GenX (genX)
    where

import Parser
import Text.PrettyPrint hiding ((<>))
import qualified Data.Set as S
import Grammar
import StrQuote

getMacroIdsFromClause :: IClause -> S.Set String
getMacroIdsFromClause (IId str) = S.singleton str
getMacroIdsFromClause (IOpt clause) = getMacroIdsFromClause clause
getMacroIdsFromClause (IPlus clause _) = getMacroIdsFromClause clause
getMacroIdsFromClause (IStar clause _) = getMacroIdsFromClause clause
getMacroIdsFromClause (ISeq clauses) = S.unions $ map getMacroIdsFromClause clauses
getMacroIdsFromClause (IAlt clauses) = S.unions $ map getMacroIdsFromClause clauses
getMacroIdsFromClause _ = S.empty

getMacroIdsHelper :: LexicalRule -> S.Set String
getMacroIdsHelper LexicalRule { getLClause = cl } = getMacroIdsFromClause cl
getMacroIdsHelper _                              = S.empty

getMacroIds :: [LexicalRule] -> S.Set String
getMacroIds lexRules = foldr (\lexRule result -> S.union result $ getMacroIdsHelper lexRule) S.empty lexRules

getSymMacroIds :: [LexicalRule] -> S.Set String
getSymMacroIds lexRules = foldr (\lexRule result ->
                                    case lexRule of
                                      MacroRule { getLRuleName = name} ->
                                        S.insert name result
                                      _ -> result)
                            S.empty
                            lexRules

genMacroText :: S.Set String -> S.Set String -> [LexicalRule] -> Doc
genMacroText sMacroIds macroIds tokens =
    vcat $ foldl (\result  lrule ->
                    case lrule of
                      (LexicalRule {getLRuleName = name, getLClause = cl }) ->
                        if name `S.member` macroIds
                          then (text "@" <> text name) <+> text "=" <+> translateClause sMacroIds cl : result
                          else result
                      (MacroRule {getLRuleName = name, getLClause = cl }) ->
                          (text "$" <> text name) <+> text "=" <+> translateClauseForMacro cl : result)
             [] tokens

genX :: NormalGrammar -> String
genX (NormalGrammar { getNGrammarName = name, getLexicalRules = tokens, getNImports = imports}) = 
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
          symMacroIds = getSymMacroIds tokens
          tokensText = genTokens symMacroIds $ removeSymmacros tokens
          macroText = genMacroText symMacroIds macroIds tokens
          adt = genTokenADT $ removeSymmacros tokens
          header = vcat [text "{", ((text "module" <+> text name) <> text "Lexer(alexScanTokens, Token(..))"), text "where", text imports, text " }", 
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

genTokens :: S.Set String -> [LexicalRule] -> Doc
genTokens smacroIds lexical_rules =
  text "tokens" <+> text ":-" <+> vcat (map makeToken lexical_rules ++ [text ". { rtkError }"])
    where makeToken LexicalRule { getLRuleDataType = data_type, getLRuleFunc = func, getLRuleName = name, getLClause = cl } =
              translateClause smacroIds cl <+> makeProduction name data_type func
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
                                                        '(' -> True
                                                        ')' -> True
                                                        ' ' -> True
                                                        '*' -> True
                                                        '/' -> True
                                                        '{' -> True
                                                        '}' -> True
                                                        '$' -> True
                                                        '"' -> True
                                                        _   -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  str)

translateClauseForMacro (IStrLit s) = text s
translateClauseForMacro (IRegExpLit re) = brackets $ text $ backquoteStrInBrackets re
translateClauseForMacro (ISeq cls) = hsep $ punctuate (text " ") (map translateClauseForMacro cls)
translateClauseForMacro (IAlt clauses) = hsep $ punctuate (text "|") (map translateClauseForMacro clauses)
translateClauseForMacro cl = text $ "Cannot translate to macro def " ++ (show cl)

translateClause sMacroIds (IId name) | name `S.member` sMacroIds =
  text "$" <> text name
translateClause sMacroIds (IId name) =
  text "@" <> text name
translateClause _ (IStrLit s)         = doubleQuotes $ text $ backquoteStr s
translateClause _ (IDot)              = text "."
translateClause _ (IRegExpLit re)     = brackets $ text $ backquoteStrInBrackets re
translateClause sMacroIds (IStar cl Nothing)  = translateClause sMacroIds cl <+> text "*"
-- a* ~x --> (a(x a)*)?
translateClause _ (IStar cl (Just _)) = error $ "Star (*) clauses with delimiters are not supported in lexical rules"
translateClause sMacroIds (IPlus cl Nothing)  = translateClause sMacroIds cl <+> text "+"
translateClause _ (IPlus cl (Just _)) = error $ "Plus (+) clauses with delimiters are not supported in lexical rules"
translateClause sMacroIds (IAlt clauses)      = parens $ hsep $ punctuate (text "|") (map (translateClause sMacroIds) clauses)
translateClause sMacroIds (ISeq clauses)    = hsep $ punctuate (text " ") (map (translateClause sMacroIds) clauses)
translateClause sMacroIds (IOpt clause)       = translateClause sMacroIds clause <+> text "?"
translateClause _ cl                 = error $ "Can't translate to lexer spec: " ++ (show cl)

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)
