{
module Lexer where
}

%wrapper "monad"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaDigit = [a-zA-Z0-9]
$dquote     = "
$squote     = '
$notsq = [^']
$notdq = [^"]
$litsym = [^"]

tokens:-

    $white+             { skip }
    "#".*               { skip }
    grammar             { simple Grammar }
    imports             { simple Imports }
    "@shortcuts"        { simple Shortcuts }
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
    $squote ($notsq | "\'")* $squote   { simple1 $ StrLit . (reverse.tail.reverse.tail) . unBackQuote }
    "[" ([^\]]|"\]")* "]"      { simple1 $ RegExpLit . (reverse.tail.reverse.tail) . unBackQuote }
    "*"                 { simple Star }
    "+"                 { simple Plus }
    $alpha $alphaDigit* { simple1 Id }
    <0> $dquote $dquote $dquote             { begin bigstring } 
    <bigstring> ($notdq | $dquote $notdq | $dquote $dquote $notdq) *                       { simple1 $ BigStr }
    <bigstring> ($notdq | $dquote $notdq | $dquote $dquote $notdq) * $dquote $             { simple1 $ BigStr }
    <bigstring> ($notdq | $dquote $notdq | $dquote $dquote $notdq) * $dquote $dquote $     { simple1 $ BigStr }
    <bigstring> $dquote $dquote $dquote     { begin 0 }

{

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
    | EndOfFile
      deriving (Eq, Show)

unBackQuote :: String -> String
unBackQuote ('\\':'n':xs) = '\\':'n' : unBackQuote xs
unBackQuote ('\\':'t':xs) = '\\':'t' : unBackQuote xs
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

simple1 t (_, _, str) len = return $ t (take len str)

simple t input len = return t

}
