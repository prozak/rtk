{
module Parser where

import qualified Lexer as L (Token(..), alexScanTokens)
import Data.Generics
import Data.Data
import Data.Char
import qualified Data.Map as Map

}

%name parse
%tokentype  { L.Token }
%error      { parseError }

%token

grammar { L.Grammar }
'='    { L.Eq }
'|'     { L.OrClause }
':'     { L.Colon }
';'     { L.RlEnd }
'*'     { L.Star }
'+'     { L.Plus }
'?'     { L.Question }
')'     { L.RParen }
'('     { L.LParen }
'.'     { L.Dot }
'!'     { L.Excl }
'~'     { L.Tilde }
','     { L.Comma }
id  { L.Id $$ }
str       { L.StrLit $$ }
rexplit       { L.RegExpLit $$ }

%%

Grammar : grammar str ';' Rules { Grammar $2 (reverse $4) }

Rules : Rule                    { [$1] } 
      | Rules Rule              { $2 : $1 }

Rule : id '=' ClauseAlt ';'         { Rule Nothing Nothing $1 $3 }
     | id ':' id '=' ClauseAlt ';'  { Rule (Just $1) Nothing $3 $5 }
     | id '.' id ':' id '=' ClauseAlt ';'  { Rule (Just $1) (Just $3) $5 $7 }
     | '.' id ':' id '=' ClauseAlt ';'  { Rule Nothing (Just $2) $4 $6 }

ClauseAlt : ClauseAlt1              { Alt (reverse $1) }

ClauseAlt1 : ClauseAlt1 '|' ClauseSeq   { $3 : $1 } 
           | ClauseSeq                  { [$1] }

ClauseSeq : ClauseSeq1              { Seq (reverse $1) }

ClauseSeq1 : ClauseSeq1 ClausePre    { $2 : $1 } 
           | {- empty -}             { [] }

ClausePre : '(' ClauseAlt ')'         { $2 }
           | ',' ClausePost           { Lifted $2 }
           | '!' ClausePost           { Ignore $2 }
           | ClausePost               { $1 }

ClausePost : ClauseItem '*' OptDelim  { Star $1 $3 }
           | ClauseItem '+' OptDelim  { Plus $1 $3 }
           | ClauseItem '?'           { Opt $1 }
           | ClauseItem               { $1 }


ClauseItem : id                       { Id $1 } 
           | str                      { StrLit $1 }
           | '.'                      { Dot }
           | rexplit                  { RegExpLit $1 }

OptDelim : {- empty -}          { Nothing }
         | '~' ClauseItem       { Just $2 }

{

parseError :: [L.Token] -> a
parseError rest = error $ "Parse error" ++ (show rest)

data Grammar a = Grammar { getGrammarName :: String, getRules :: [Rule a] }
                 deriving (Eq, Show, Typeable, Data)

data Rule a = Rule { getDataTypeName :: a, getDataFunc :: a, getRuleName :: String, getClause :: Clause }
                deriving (Eq, Show, Typeable, Data)

data Clause = Id { getIdStr :: String }
            | StrLit String
            | Dot
            | RegExpLit String
            | Star Clause (Maybe Clause)
            | Plus Clause (Maybe Clause)
            | Alt [Clause]
            | Seq [Clause]
            | Opt Clause
            | Lifted Clause
            | Ignore Clause
              deriving (Eq, Show, Typeable, Data)

type InitialGrammar = Grammar (Maybe String) 
type NormalGrammar = Grammar String
type NormalRule = Rule String

isLexicalRule :: String -> Bool
isLexicalRule [] = False
isLexicalRule (c:_) = isLower c

}
