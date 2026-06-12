{
module Lexer where

import Data.Data (Data)
import Diagnostics (Diagnostic, diagnosticFromPositioned, renderDiagnostic)
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
-- grammar.pg's id rule is [a-zA-Z][A-Za-z0-9_]*: underscores continue an
-- identifier. The reference lexer follows the spec (it used to reject '_',
-- a divergence the generated front end never had).
$alphaDigit = [a-zA-Z0-9_]
$dq     = "
$squote     = '
$notsq = [^']
$notdq = [^"]
$litsym = [^"]

tokens:-

    $white+             { skip }
    "#".*               { skip }
    <0>"/*"             { beginMultiLineComment }
    <mlcomment> "/*"    { beginMultiLineComment }
    <mlcomment> "*/"    { tryEndMultiLineComment }
    <mlcomment>([^\*\/]|[\*][^\/]|[\/][^\*])* { skip }
    grammar             { simple Grammar }
    imports             { simple Imports }
    "@shortcuts"        { simple Shortcuts }
    "@symmacro"         { simple Symmacro }
    "="                 { simple Eq }
    ";"                 { simple RlEnd }
    ":"                 { simple Colon }
    "|"                 { simple OrClause }
    "."                 { simple Dot }
    "?"                 { simple  Question }
    ","                 { simple Comma }
    "!"                 { simple  Excl }
    "~"                 { simple  Tilde }
    "$"                 { simple  Dollar }
    ")"                 { simple  RParen }
    "("                 { simple  LParen }
    -- Literal tokens keep the FULL match (delimiters and escape pairs
    -- intact), exactly like the generated lexer: the shared
    -- Frontend.cleanGrammarTokens pass strips delimiters and processes
    -- escapes on the parsed AST, so the cleanup logic exists only once.
    $squote ([^'\\] | \\ .)* $squote   { simple1 StrLit }
    "[" ([^\]]|"\]")* "]"      { simple1 RegExpLit }
    "*"                 { simple Star }
    "+"                 { simple Plus }
    $alpha $alphaDigit* { simple1 Id }
    $dq $dq $dq ($notdq|$dq $notdq | $dq $dq $notdq | [\n])* $dq $dq $dq { simple1 BigStr }
    .                                       { rtkError }

{

getStateCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, getCommentDepth ust)
setStateCommentDepth i = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{getCommentDepth = i}}, ()) 

beginMultiLineComment :: AlexInput -> Int -> Alex PosToken
beginMultiLineComment alexInput i =
  do
    commentDepth <- getStateCommentDepth
    setStateCommentDepth $ commentDepth + 1
    alexSetStartCode mlcomment
    skip alexInput i

tryEndMultiLineComment :: AlexInput -> Int -> Alex PosToken
tryEndMultiLineComment alexInput i =
  do
    commentDepth <- getStateCommentDepth
    setStateCommentDepth $ commentDepth - 1
    if (commentDepth - 1 == 0)
      then
        do
          alexSetStartCode 0
          skip alexInput i
      else
        skip alexInput i

data AlexUserState = AlexUserState {getCommentDepth :: Integer}

alexInitUserState = AlexUserState { getCommentDepth = 0 }

data Token = Grammar 
    | Imports
    | Eq 
    | RlEnd 
    | OrClause 
    | Dot 
    | RegExpLit String 
    | StrLit String
    | BigStr String 
    | Id String 
    | Star 
    | Plus
    | Excl
    | Comma
    | RParen
    | LParen
    | Dollar
    | Question
    | Colon
    | Tilde
    | Shortcuts
    | Symmacro
    | EndOfFile
      deriving (Eq, Show, Data)

-- AlexPosn is defined by the alex wrapper, so a Data instance can only be
-- attached via standalone deriving; profiling deep-forces token lists
-- through it (see Debug.deepForce)
deriving instance Data AlexPosn

-- A token together with the source position where it starts
data PosToken = PosToken { ptPos :: AlexPosn, ptToken :: Token }
                deriving (Eq, Show, Data)

-- Lex the input into a token stream, returning a structured diagnostic on a
-- lexical error. The returned list always ends with an EndOfFile token that
-- carries the position of the end of input, so parse errors at end of input
-- can be reported with a position too.
-- rtkError encodes the error position as "LINE:COL:message";
-- diagnosticFromPositioned recovers it so the diagnostic carries a real
-- SourcePos.
scanTokens :: String -> Either Diagnostic [PosToken]
scanTokens str =
               case alexScanTokens1 str of
                  Right toks -> Right toks
                  Left err   -> Left (diagnosticFromPositioned err)

-- Thin wrapper kept during the migration to structured diagnostics: callers
-- that have not yet switched to 'scanTokens' get the rendered message thrown.
alexScanTokens :: String -> [PosToken]
alexScanTokens str = either (errorWithoutStackTrace . renderDiagnostic "<input>") id (scanTokens str)

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       PosToken _ EndOfFile -> return $ reverse (tok : toks)
                       _ -> let toks' = tok : toks
                            in toks' `seq` loop toks'
  loop []

alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ PosToken pos EndOfFile

-- Encode the position as "LINE:COL:message" so scanTokens can split it back
-- out into a SourcePos for the diagnostic.
rtkError ((AlexPn _ line column), _, _, str) _ = alexError $ (show line) ++ ":" ++ (show column) ++ ":lexical error. Following chars: " ++ (take 10 str)

simple1 :: (String -> Token) -> AlexInput -> Int -> Alex PosToken
simple1 t (pos, _, _, str) len = return $ PosToken pos (t (take len str))

simple :: Token -> AlexInput -> Int -> Alex PosToken
simple t (pos, _, _, _) _ = return $ PosToken pos t

}
