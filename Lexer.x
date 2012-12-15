{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaDigit = [a-zA-Z0-9]
$dquote     = "
$squote     = '
$notsq = [^']
$notdq = [^"]
$litsym = [^"]

tokens:-

    $white+             ;
    "#".*               ;
    "/*"([\n]|.|[\r])*"*/"          ;
    grammar             {\_ -> Grammar }
    "="                {\_ -> Eq }
    ";"                 {\_ -> RlEnd }
    ":"                 {\_ -> Colon }
    "|"                 {\_ -> OrClause }
    "."                 {\_ -> Dot }
    "?"                 {\_ ->  Question }
    ","                 {\_ -> Comma }
    "!"                 {\_ ->  Excl }
    "~"                 {\_ ->  Tilde }
    "$"                 {\_ ->  Dollar }
    ")"                 {\_ ->  RParen }
    "("                 {\_ ->  LParen }
    $squote ($notsq | "\'")* $squote   {\s ->  StrLit $ (reverse.tail.reverse.tail) $ unBackQuote s }
    "[" ([^\]]|"\]")* "]"      {\s -> RegExpLit $ (reverse.tail.reverse.tail) $ unBackQuote s }
    "*"                 {\s -> Star }
    "+"                 {\s -> Plus }
    $alpha $alphaDigit* {\s -> Id s}

{

data Token = Grammar 
    | Eq 
    | RlEnd 
    | OrClause 
    | Dot 
    | RegExpLit String 
    | StrLit String 
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
      deriving (Eq, Show)

unBackQuote :: String -> String
unBackQuote ('\\':'n':xs) = '\\':'n' : unBackQuote xs
unBackQuote ('\\':'t':xs) = '\\':'t' : unBackQuote xs
unBackQuote ('\\':c:xs) = c : unBackQuote xs
unBackQuote (c:xs) = c : unBackQuote xs
unBackQuote [] = []

}
