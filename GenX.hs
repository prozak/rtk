{-# LANGUAGE QuasiQuotes #-}
module GenX (genX, isAlexEscape)
    where

import qualified GrammarParser as GP

import Syntax
import Diagnostics (Diagnostic(..))
import Frontend (altElems, nameText, seqElems, showClause, strLitText)
import Text.PrettyPrint hiding ((<>))
import qualified Data.Set as S
import Data.Maybe (catMaybes)
import Grammar
import StrQuote

getMacroIdsFromClause :: LClause -> S.Set String
getMacroIdsFromClause (GP.Ref _ n) = S.singleton (nameText n)
getMacroIdsFromClause (GP.Opt _ clause) = getMacroIdsFromClause clause
getMacroIdsFromClause (GP.Plus _ clause) = getMacroIdsFromClause clause
getMacroIdsFromClause (GP.PlusDelim _ clause _) = getMacroIdsFromClause clause
getMacroIdsFromClause (GP.Star _ clause) = getMacroIdsFromClause clause
getMacroIdsFromClause (GP.StarDelim _ clause _) = getMacroIdsFromClause clause
getMacroIdsFromClause (GP.Seq _ l r) = getMacroIdsFromClause l `S.union` getMacroIdsFromClause r
getMacroIdsFromClause (GP.Alt _ l r) = getMacroIdsFromClause l `S.union` getMacroIdsFromClause r
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

-- The output lines are emitted in reverse token order, matching the original
-- foldl; keep it so the generated lexer is byte-for-byte unchanged.
genMacroText :: S.Set String -> S.Set String -> [LexicalRule] -> Either Diagnostic Doc
genMacroText sMacroIds macroIds tokens = do
    docs <- mapM lineFor tokens
    return $ vcat $ reverse $ catMaybes docs
  where
    lineFor (LexicalRule {getLRuleName = name, getLClause = cl})
      | name `S.member` macroIds = do
          d <- translateRuleClause name sMacroIds cl
          return $ Just $ (text "@" <> text name) <+> text "=" <+> d
      | otherwise = return Nothing
    lineFor (MacroRule {getLRuleName = name, getLClause = cl}) = do
          d <- translateClauseForMacro name cl
          return $ Just $ (text "$" <> text name) <+> text "=" <+> d

genX :: NormalGrammar -> Either Diagnostic String
genX (NormalGrammar { getNGrammarName = name, getLexicalRules = tokens, getNImports = imports}) = do
    tokensText <- genTokens symMacroIds $ removeSymmacros tokens
    macroText <- genMacroText symMacroIds macroIds tokens
    return $ render $ vcat [
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
          adt = genTokenADT $ removeSymmacros tokens
          header = vcat [text (provenanceBanner name),
                         text "{",
                         text "{-# LANGUAGE StandaloneDeriving, DeriveDataTypeable #-}",
                         ((text "module" <+> text name) <> text "Lexer(scanTokens, alexScanTokens, Token(..), PosToken(..), AlexPosn(..))"), text "where",
                         text "import Data.Data (Data)",
                         text imports, text " }",
                         text "%wrapper \"monad\""]
          funs_text = [str|
-- AlexPosn is defined by the alex wrapper, so the Data instance (required by
-- the parser's RtkPos position type) can only be attached via standalone
-- deriving - the same solution as the hand-written Lexer.x
deriving instance Data AlexPosn

-- A token together with the source position where it starts
data PosToken = PosToken { ptPos :: AlexPosn, ptToken :: Token }
                deriving (Show)

alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ PosToken pos EndOfFile

-- Lex the input into a token stream, returning the positioned error message
-- on a lexical error (encoded as "LINE:COL:message", see rtkError below).
-- The returned list always ends with an EndOfFile token that carries the
-- position of the end of input, so parse errors at end of input can be
-- reported with a position too
scanTokens :: String -> Either String [PosToken]
scanTokens str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       PosToken _ EndOfFile -> return $ reverse (tok : toks)
                       _ -> let toks' = tok : toks
                            in toks' `seq` loop toks'
  loop []

-- Thin compatibility wrapper: callers that have not switched to 'scanTokens'
-- get the error message thrown instead
alexScanTokens :: String -> [PosToken]
alexScanTokens str =
               case scanTokens str of
                  Right toks -> toks
                  Left err -> errorWithoutStackTrace err

simple1 :: (String -> Token) -> AlexInput -> Int -> Alex PosToken
simple1 t (pos, _, _, str) len = return $ PosToken pos (t (take len str))

simple :: Token -> AlexInput -> Int -> Alex PosToken
simple t (pos, _, _, _) len = return $ PosToken pos t

-- Encode the position as "LINE:COL:message" so callers can split it back out
-- into a structured position - the same encoding the rtk grammar lexer uses
rtkError ((AlexPn _ line column), _, _, str) len = alexError $ (show line) ++ ":" ++ (show column) ++ ":lexical error. Following chars: " ++ (take 10 str)
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

genTokens :: S.Set String -> [LexicalRule] -> Either Diagnostic Doc
genTokens smacroIds lexical_rules = do
    toks <- mapM makeToken lexical_rules
    return $ text "tokens" <+> text ":-" <+> vcat (toks ++ [text ". { rtkError }"])
    where makeToken LexicalRule { getLRuleDataType = data_type, getLRuleFunc = func, getLRuleName = name, getLClause = cl } = do
              cld <- translateRuleClause name smacroIds cl
              return $ cld <+> makeProduction name data_type func
          makeToken (MacroRule _ _) = return empty
          makeProduction name data_type func =
            let token_name = text $ tokenName name in
              case data_type of
                   "Keyword" -> text "{ simple" <+> token_name <+> text "}"
                   "Ignore"  -> text ";"
                   _         -> text "{ simple1 $ " <+> token_name <+> text "." <+> (parens $ text func) <+> text "}"

-- Alex quoted strings are literal: backslash is not an escape character
-- inside "..." (e.g. "\" matches a single backslash, "\'" matches a
-- backslash followed by a quote), so only the string terminator '"' needs
-- rewriting. Do not "fix" this to escape backslashes: doubling them
-- changes what the token matches.
backquoteStr :: String -> String
backquoteStr s = concat (map (\chr -> if (case chr of
                                                 '"'  -> True
                                                 _    -> False)
                                          then ['\\', chr]
                                          else [chr] )
                                  s)

-- Escape the characters that are special in Alex set syntax ([...]). A
-- literal backslash is special too and must become \\: it used to pass
-- through raw, where Alex reads it as an escape, so a backslash silently
-- changed the meaning of its class and users resorted to the [\x5C] hex
-- spelling (issue #95). The exception: token post-processing
-- (TokenProcessing.unBackQuote) keeps the \n \t \r \f \v escape pairs
-- intact in regex content, so a backslash heading one of those pairs
-- stands for the control character and passes through for Alex to read
-- the same way.
backquoteStrInBrackets :: String -> String
backquoteStrInBrackets ('\\' : c : rest)
    | isAlexEscape ['\\', c] = '\\' : c : backquoteStrInBrackets rest
backquoteStrInBrackets (chr : rest) = escaped ++ backquoteStrInBrackets rest
    where escaped | chr `elem` "[]() */{}$\"\\" = ['\\', chr]
                  | otherwise                   = [chr]
backquoteStrInBrackets [] = []

-- Report a lexical-rule generation error, naming the rule for context.
lexErr :: ID -> String -> Either Diagnostic a
lexErr rname msg = Left $ Diagnostic Nothing (Just ("in lexical rule '" ++ rname ++ "'")) msg

-- A @symmacro definition. Mirrors the rule-clause translation but renders
-- alternations bare (no parentheses) and rejects everything that is not a
-- literal, a regex or a grouping of those.
translateClauseForMacro :: ID -> LClause -> Either Diagnostic Doc
translateClauseForMacro _ (GP.Lit _ s) = Right $ text (strLitText s)
translateClauseForMacro _ (GP.Regex _ re) = Right $ brackets $ text $ backquoteStrInBrackets re
translateClauseForMacro rname c@GP.Seq{} = do
    ds <- mapM (translateClauseForMacro rname) (seqElems c)
    return $ hsep $ punctuate (text " ") ds
translateClauseForMacro rname c@GP.Alt{} = do
    ds <- mapM (translateClauseForMacro rname) (altElems c)
    return $ hsep $ punctuate (text "|") ds
translateClauseForMacro rname cl = lexErr rname $ "cannot translate clause to a lexer macro definition: " ++ showClause cl

-- Detect Alex escape sequences that should be output as bare escapes, not quoted strings.
-- In Alex: "\n" = literal backslash+n, but \n = newline character.
-- When grammar has '\n', we want to generate \n (the escape), not "\n" (literal).
-- This set must stay identical to the escapes TokenProcessing.unBackQuote
-- preserves, or escapes get silently stripped before they reach this point.
isAlexEscape :: String -> Bool
isAlexEscape "\\n" = True   -- newline
isAlexEscape "\\t" = True   -- tab
isAlexEscape "\\r" = True   -- carriage return
isAlexEscape "\\f" = True   -- form feed
isAlexEscape "\\v" = True   -- vertical tab
isAlexEscape _ = False

-- The whole clause of a lexical rule. A user rule's clause renders as a
-- parenthesized alternation (the same canonical wrapping the retired
-- IClause front half applied to every rule); the synthesized keyword rules
-- (string-literal tokens, start-wrapper dummies) have a bare literal clause
-- and render as a bare quoted string.
translateRuleClause :: ID -> S.Set ID -> LClause -> Either Diagnostic Doc
translateRuleClause rname sMacroIds cl = case cl of
    GP.Lit{} -> translateItem rname sMacroIds cl
    _        -> translateTop rname sMacroIds cl

-- A clause in rule (or parenthesized-group) position: a parenthesized
-- alternation of space-separated sequences.
translateTop :: ID -> S.Set ID -> LClause -> Either Diagnostic Doc
translateTop rname sMacroIds c = do
    ds <- mapM (translateAlt rname sMacroIds) (altElems c)
    return $ parens $ hsep $ punctuate (text "|") ds

translateAlt :: ID -> S.Set ID -> LClause -> Either Diagnostic Doc
translateAlt rname sMacroIds alt = do
    ds <- mapM (translateElem rname sMacroIds) (seqElems alt)
    return $ hsep $ punctuate (text " ") ds

translateElem :: ID -> S.Set ID -> LClause -> Either Diagnostic Doc
translateElem rname sMacroIds c = case c of
    GP.Star _ cl       -> (<> text "*") <$> translateItem rname sMacroIds cl
    GP.StarDelim{}     -> lexErr rname "star (*) clauses with delimiters (~) are not supported in lexical rules"
    GP.Plus _ cl       -> (<> text "+") <$> translateItem rname sMacroIds cl
    GP.PlusDelim{}     -> lexErr rname "plus (+) clauses with delimiters (~) are not supported in lexical rules"
    GP.Opt _ cl        -> (<+> text "?") <$> translateItem rname sMacroIds cl
    GP.Lifted{}        -> cannotTranslate rname c
    GP.Ignored{}       -> cannotTranslate rname c
    GP.Labeled{}       -> cannotTranslate rname c
    GP.Anti_Clause{}   -> cannotTranslate rname c
    _                  -> translateItem rname sMacroIds c

-- An item (a repetition operand or a sequence element): a leaf, or a
-- parenthesized group.
translateItem :: ID -> S.Set ID -> LClause -> Either Diagnostic Doc
translateItem _ sMacroIds (GP.Ref _ n)
  | nameText n `S.member` sMacroIds = Right $ text "$" <> text (nameText n)
  | otherwise                       = Right $ text "@" <> text (nameText n)
translateItem _ _ (GP.Lit _ s)
  | isAlexEscape lit = Right $ text lit   -- output bare escape: \n, \t, etc.
  | otherwise        = Right $ doubleQuotes $ text $ backquoteStr lit
  where lit = strLitText s
translateItem _ _ (GP.Dot _)      = Right $ text "."
translateItem _ _ (GP.Regex _ re) = Right $ brackets $ text $ backquoteStrInBrackets re
translateItem rname sMacroIds c   = translateTop rname sMacroIds c

cannotTranslate :: ID -> LClause -> Either Diagnostic a
cannotTranslate rname cl = lexErr rname $ "cannot translate clause to lexer spec: " ++ showClause cl

joinAlts :: [Doc] -> Doc
joinAlts alts = vcat $ punctuate (text " |") (filter (not.isEmpty) alts)
