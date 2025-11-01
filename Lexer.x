{
module Lexer where
}

%wrapper "monadUserState"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaDigit = [a-zA-Z0-9]
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
    $squote ($notsq | "\'")* $squote   { simple1 $ StrLit . (reverse.drop 1.reverse.drop 1) . unBackQuote }
    "[" ([^\]]|"\]")* "]"      { simple1 $ RegExpLit . (reverse.drop 1.reverse.drop 1) . unBackQuote }
    "*"                 { simple Star }
    "+"                 { simple Plus }
    $alpha $alphaDigit* { simple1 Id }
    $dq $dq $dq ($notdq|$dq $notdq | $dq $dq $notdq | [\n])* $dq $dq $dq { simple1 $ BigStr . (reverse.(drop 3).reverse.(drop 3))} 
    .                                       { rtkError }

{

getStateCommentDepth = Alex $ \s@AlexState{alex_ust=ust} -> Right (s, getCommentDepth ust)
setStateCommentDepth i = Alex $ \s@AlexState{alex_ust=ust} -> Right (s{alex_ust=ust{getCommentDepth = i}}, ()) 

beginMultiLineComment :: AlexInput -> Int -> Alex Token
beginMultiLineComment alexInput i =
  do
    commentDepth <- getStateCommentDepth
    setStateCommentDepth $ commentDepth + 1
    alexSetStartCode mlcomment
    skip alexInput i

tryEndMultiLineComment :: AlexInput -> Int -> Alex Token    
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
      deriving (Eq, Show)

unBackQuote :: String -> String
unBackQuote ('\\':'n':xs) = '\\':'n' : unBackQuote xs
unBackQuote ('\\':'t':xs) = '\\':'t' : unBackQuote xs
unBackQuote ('\\':'r':xs) = '\\':'r' : unBackQuote xs
unBackQuote ('\\':c:xs) = c : unBackQuote xs
unBackQuote (c:xs) = c : unBackQuote xs
unBackQuote [] = []

alexScanTokens :: String -> [Token]
alexScanTokens str = 
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> error err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       EndOfFile -> return $ catBigstrs $ reverse toks
                       _ -> let toks' = tok : toks 
                            in toks' `seq` loop toks'
  loop []

catBigstrs :: [Token] -> [Token]
catBigstrs (BigStr s1 : toks) = case catBigstrs toks of
                (BigStr s2 : toks') -> (BigStr (s1 ++ ('\n' : s2)) : toks')
                _ -> BigStr s1 : toks
catBigstrs (tok : toks) = tok : catBigstrs toks
catBigstrs [] = []

alexEOF = return EndOfFile

rtkError ((AlexPn _ line column), _, _, str) _ = alexError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column" ++ ". Following chars :" ++ (take 10 str)

simple1 :: (String -> Token) -> AlexInput -> Int -> Alex Token
simple1 t (_, _, _, str) len = return $ t (take len str)

simple t _ _ = return t

}
