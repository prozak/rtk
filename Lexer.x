{
module Lexer where
}

%wrapper "basic"

$digit = 0-9
$alpha = [a-zA-Z]
$alphaDigit = [a-zA-Z0-9]
$quote     = "
$notq = [^"]

tokens:-

    $white+             ;
    "#".*               ;
    grammar             {\_ -> Grammar }
    ":=="               {\_ -> Eqn }
    ":="                {\_ -> Eq }
    ";"                 {\_ -> RlEnd }
    "|"                 {\_ -> OrClause }
    "."                 {\_ -> Dot }
    "[" [^\]]* "]"      {\s -> RegExpLit s }
    $quote $notq* $quote    {\s -> StrLit $ reverse $ tail $ reverse $ tail s }
    "*"                 {\s -> Star }
    $alpha $alphaDigit* {\s -> Id s}

{

data Token = 
    Grammar |
    Eqn |
    Eq |
    RlEnd |
    OrClause |
    Dot |
    RegExpLit String |
    StrLit String |
    Id String |
    Star deriving (Eq, Show)

}
