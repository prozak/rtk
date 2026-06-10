{-# LANGUAGE QuasiQuotes #-}
module GenX (genX)
    where

import Parser
import Text.PrettyPrint hiding ((<>))
import qualified Data.Set as S
import Grammar
import StrQuote

getMacroIdsFromClause :: IClause -> S.Set String
getMacroIdsFromClause (IId s) = S.singleton s
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
                          then (text "@" <> text name) <+> text "=" <+> translateClause name sMacroIds cl : result
                          else result
                      (MacroRule {getLRuleName = name, getLClause = cl }) ->
                          (text "$" <> text name) <+> text "=" <+> translateClauseForMacro name cl : result)
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
          header = vcat [text "{", ((text "module" <+> text name) <> text "Lexer(alexScanTokens, Token(..), PosToken(..), AlexPosn(..))"), text "where", text imports, text " }",
                         text "%wrapper \"monad\""]
          funs_text = [str|
-- A token together with the source position where it starts
data PosToken = PosToken { ptPos :: AlexPosn, ptToken :: Token }
                deriving (Show)

alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ PosToken pos EndOfFile

-- The returned list always ends with an EndOfFile token that carries the
-- position of the end of input, so parse errors at end of input can be
-- reported with a position too
alexScanTokens :: String -> [PosToken]
alexScanTokens str =
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> errorWithoutStackTrace err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       PosToken _ EndOfFile -> return $ reverse (tok : toks)
                       _ -> let toks' = tok : toks
                            in toks' `seq` loop toks'
  loop []

simple1 :: (String -> Token) -> AlexInput -> Int -> Alex PosToken
simple1 t (pos, _, _, str) len = return $ PosToken pos (t (take len str))

simple :: Token -> AlexInput -> Int -> Alex PosToken
simple t (pos, _, _, _) len = return $ PosToken pos t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ". Following chars: " ++ (take 10 str)
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
          makeToken (MacroRule _ _) = empty

genTokens :: S.Set String -> [LexicalRule] -> Doc
genTokens smacroIds lexical_rules =
  text "tokens" <+> text ":-" <+> vcat (map makeToken lexical_rules ++ [text ". { rtkError }"])
    where makeToken LexicalRule { getLRuleDataType = data_type, getLRuleFunc = func, getLRuleName = name, getLClause = cl } =
              translateClause name smacroIds cl <+> makeProduction name data_type func
          makeToken (MacroRule _ _) = empty
          makeProduction name data_type func =
            let token_name = text $ tokenName name in
              case data_type of
                   "Keyword" -> text "{ simple" <+> token_name <+> text "}"
                   "Ignore"  -> text ";"
                   _         -> text "{ simple1 $ " <+> token_name <+> text "." <+> (parens $ text func) <+> text "}"

backquoteStr :: String -> String
backquoteStr s = concat (map (\chr -> if (case chr of
                                                 '"'  -> True
                                                 _    -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  s)

backquoteStrInBrackets :: String -> String
backquoteStrInBrackets s = concat (map (\chr -> if (case chr of
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
                                  s)

translateClauseForMacro :: ID -> IClause -> Doc
translateClauseForMacro _ (IStrLit s) = text s
translateClauseForMacro _ (IRegExpLit re) = brackets $ text $ backquoteStrInBrackets re
translateClauseForMacro rname (ISeq cls) = hsep $ punctuate (text " ") (map (translateClauseForMacro rname) cls)
translateClauseForMacro rname (IAlt clauses) = hsep $ punctuate (text "|") (map (translateClauseForMacro rname) clauses)
translateClauseForMacro rname cl = errorWithoutStackTrace $ "In lexical rule '" ++ rname ++ "': cannot translate clause to a lexer macro definition: " ++ showClause cl

-- Detect Alex escape sequences that should be output as bare escapes, not quoted strings.
-- In Alex: "\n" = literal backslash+n, but \n = newline character.
-- When grammar has '\n', we want to generate \n (the escape), not "\n" (literal).
isAlexEscape :: String -> Bool
isAlexEscape "\\n" = True   -- newline
isAlexEscape "\\t" = True   -- tab
isAlexEscape "\\r" = True   -- carriage return
isAlexEscape "\\f" = True   -- form feed
isAlexEscape "\\v" = True   -- vertical tab
isAlexEscape _ = False

translateClause :: ID -> S.Set ID -> IClause -> Doc
translateClause _ sMacroIds (IId name) | name `S.member` sMacroIds =
  text "$" <> text name
translateClause _ _ (IId name) =
  text "@" <> text name
translateClause _ _ (IStrLit s)
  | isAlexEscape s = text s   -- output bare escape: \n, \t, etc.
  | otherwise      = doubleQuotes $ text $ backquoteStr s
translateClause _ _ (IDot)              = text "."
translateClause _ _ (IRegExpLit re)     = brackets $ text $ backquoteStrInBrackets re
translateClause rname sMacroIds (IStar cl Nothing)  = translateClause rname sMacroIds cl <> text "*"
-- a* ~x --> (a(x a)*)?
translateClause rname _ (IStar _ (Just _)) = errorWithoutStackTrace $ "In lexical rule '" ++ rname ++ "': star (*) clauses with delimiters (~) are not supported in lexical rules"
translateClause rname sMacroIds (IPlus cl Nothing)  = translateClause rname sMacroIds cl <> text "+"
translateClause rname _ (IPlus _ (Just _)) = errorWithoutStackTrace $ "In lexical rule '" ++ rname ++ "': plus (+) clauses with delimiters (~) are not supported in lexical rules"
translateClause rname sMacroIds (IAlt clauses)      = parens $ hsep $ punctuate (text "|") (map (translateClause rname sMacroIds) clauses)
translateClause rname sMacroIds (ISeq clauses)    = hsep $ punctuate (text " ") (map (translateClause rname sMacroIds) clauses)
translateClause rname sMacroIds (IOpt clause)       = translateClause rname sMacroIds clause <+> text "?"
translateClause rname _ cl                 = errorWithoutStackTrace $ "In lexical rule '" ++ rname ++ "': cannot translate clause to lexer spec: " ++ showClause cl

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)
