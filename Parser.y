{
module Parser where

import qualified Lexer as L (Token(..), alexScanTokens)
import Data.Generics
import Data.Data

}

%name parse
%tokentype  { L.Token }
%error      { parseError }

%token

grammar { L.Grammar }
':='    { L.Eq }
':=='    { L.Eqn }
'|'     { L.OrClause }
';'     { L.RlEnd }
'*'     { L.Star }
'.'     { L.Dot }
id  { L.Id $$ }
str       { L.StrLit $$ }
rexplit       { L.RegExpLit $$ }

%%

Grammar : grammar str ';' Rules { Grammar $2 $4 }

Rules : {[]} | Rule ';' Rules { $1 : $3 }

Rule : id ':=' Clauses { Rule (Id $1) $3 False } | id ':==' Clauses { Rule (Id $1) $3 True }

Clauses : Clause ClausesEnd {$1 : $2}

ClausesEnd : {[]} | '|' Clause ClausesEnd { $2 : $3 }

Clause : {([], GInfo "")} | ClauseItem Clause {($1 : (fst $2), GInfo "")}

ClauseItem : id { Id $1 } | str { StrLit $1 (LexerInfo "") } | rexplit {RegExpLit $1} | '.' { Dot } | '*' { Star }
{

parseError :: [L.Token] -> a
parseError rest = error $ "Parse error" ++ (show rest)

data Grammar = Grammar String [Rule] deriving (Eq, Show, Typeable)
data Rule = Rule { getRuleName :: ClauseItem, getClauses :: [([ClauseItem],GInfo)], buildNode :: Bool } deriving (Eq, Show)
data ClauseItem = Id { getIdStr :: String } | StrLit String LexerInfo | RegExpLit String | Star | Dot deriving (Eq, Show)
data GInfo = GInfo { clauseName :: String } deriving (Eq, Show)
data LexerInfo = LexerInfo { lexName :: String } deriving (Eq, Show)

}
