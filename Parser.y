{
module Parser where

import qualified Lexer as L (Token(..), alexScanTokens)
import Data.Generics
import Data.Data
import qualified Data.Map as Map

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
'+'     { L.Plus }
'.'     { L.Dot }
id  { L.Id $$ }
str       { L.StrLit $$ }
rexplit       { L.RegExpLit $$ }

%%

Grammar : grammar str ';' Rules { Grammar $2 $4 }

Rules : {[]} | Rule ';' Rules { $1 : $3 }

Rule : id ':=' Clauses { Rule (Id $1) (map (addRuleName $1) $3) False }
     | id ':==' Clauses { Rule (Id $1) (map (addRuleName $1) $3) True }

Clauses : Clause ClausesEnd {$1 : $2}

ClausesEnd : {[]} | '|' Clause ClausesEnd { $2 : $3 }

Clause : {([], GInfo "" "")} | ClauseItem Clause {($1 : (fst $2), GInfo "" "")}

ClauseItem : id { Id $1 } | rexplit {RegExpLit $1} | '.' { Dot } 
           | '*' { Star } | '+' { Plus } | str { StrLit $1 } 
{

parseError :: [L.Token] -> a
parseError rest = error $ "Parse error" ++ (show rest)

addRuleName :: String -> ([ClauseItem], GInfo) -> ([ClauseItem], GInfo)
addRuleName ruleName (clauses, info) = (clauses, info{ruleName = ruleName})

data Grammar = Grammar { getGrammarName :: String, getRules :: [Rule] }
                 deriving (Eq, Show, Typeable, Data)

data Rule = Rule { getRuleName :: ClauseItem, getClauses :: [([ClauseItem],GInfo)], buildNode :: Bool }
              deriving (Eq, Show, Typeable, Data)

data ClauseItem = Id { getIdStr :: String } | StrLit String | RegExpLit String | Dot 
                   | Star
                   | Plus
                   | LoopStar ClauseItem (Maybe ClauseItem)
                   | LoopPlus ClauseItem (Maybe ClauseItem)
                   deriving (Eq, Show, Typeable, Data)

data GInfo = GInfo { clauseName :: String, ruleName :: String } deriving (Eq, Show, Typeable, Data)
mkGInfo = GInfo "" ""

}
