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

Grammar : grammar str ';' Rules { InitialGrammar $2 (reverse $4) }

Rules : Rule                    { [$1] } 
      | Rules Rule              { $2 : $1 }

Rule : id '=' ClauseAlt ';'         { IRule Nothing Nothing $1 $3 }
     | id ':' id '=' ClauseAlt ';'  { IRule (Just $1) Nothing $3 $5 }
     | id '.' id ':' id '=' ClauseAlt ';'  { IRule (Just $1) (Just $3) $5 $7 }
     | '.' id ':' id '=' ClauseAlt ';'  { IRule Nothing (Just $2) $4 $6 }

ClauseAlt : ClauseAlt1              { IAlt (reverse $1) }

ClauseAlt1 : ClauseAlt1 '|' ClauseSeq   { $3 : $1 } 
           | ClauseSeq                  { [$1] }

ClauseSeq : ClauseSeq1              { ISeq (reverse $1) }

ClauseSeq1 : ClauseSeq1 ClausePre    { $2 : $1 } 
           | {- empty -}             { [] }

ClausePre :  ',' ClausePost           { ILifted $2 }
           | '!' ClausePost           { IIgnore $2 }
           | ClausePost               { $1 }

ClausePost : ClauseItem '*' OptDelim  { IStar $1 $3 }
           | ClauseItem '+' OptDelim  { IPlus $1 $3 }
           | ClauseItem '?'           { IOpt $1 }
           | ClauseItem               { $1 }


ClauseItem : '(' ClauseAlt ')'        { $2 }
           | id                       { IId $1 } 
           | str                      { IStrLit $1 }
           | '.'                      { IDot }
           | rexplit                  { IRegExpLit $1 }

OptDelim : {- empty -}          { Nothing }
         | '~' ClauseItem       { Just $2 }

{

parseError :: [L.Token] -> a
parseError rest = error $ "Parse error" ++ (show rest)

data InitialGrammar = InitialGrammar { getIGrammarName :: String, getIRules :: [IRule] }
                 deriving (Eq, Show, Typeable, Data)

data IRule = IRule { getIDataTypeName :: (Maybe String), 
                     getIDataFunc :: (Maybe String), 
                     getIRuleName :: String, 
                     getIClause :: IClause }
                  deriving (Eq, Show, Typeable, Data)

type ConstructorName = String

type ID = String

data IClause = IId { getIdStr :: ID }
             | IStrLit String
             | IDot
             | IRegExpLit String
             | IStar IClause (Maybe IClause)
             | IPlus IClause (Maybe IClause)
             | IAlt [IClause]
             | ISeq [IClause]
             | IOpt IClause
             | ILifted IClause
             | IIgnore IClause
              deriving (Eq, Show, Typeable, Data)

data NormalGrammar = NormalGrammar { getNGrammarName :: String, 
                                     getSyntaxRuleGroups :: [SyntaxRuleGroup], 
                                     getLexicalRules :: [LexicalRule] }
                     deriving (Eq, Show, Typeable, Data)

data SyntaxRuleGroup = SyntaxRuleGroup { getSDataTypeName :: ID,
                                         getSRules :: [SyntaxRule]}
                       deriving (Eq, Show, Typeable, Data)

data SyntaxRule = SyntaxRule { getSRuleName :: ID,
                               getSClause :: SyntaxTopClause}
                       deriving (Eq, Show, Typeable, Data)

data STManyOp = STStar
              | STPlus
                deriving (Eq, Show, Typeable, Data)

data STSeq = STSeq ConstructorName [SyntaxSimpleClause]
             deriving (Eq, Show, Typeable, Data)

data SyntaxTopClause = STMany STManyOp SyntaxSimpleClause (Maybe SyntaxSimpleClause)
                     | STOpt SyntaxSimpleClause
                     | STAltOfSeq { getAltOfSeq :: [STSeq] } -- alternative of sequences
                       deriving (Eq, Show, Typeable, Data)
                                   
data SyntaxSimpleClause = SSId ID
                        | SSLifted ID
                        | SSIgnore ID
                          deriving (Eq, Show, Typeable, Data)

data LexicalRule = LexicalRule { getLRuleDataType :: String, 
                                 getLRuleFunc :: String, 
                                 getLRuleName :: String, 
                                 getLClause :: LClause}
                   deriving (Eq, Show, Typeable, Data)

type LClause = IClause

isLexicalRule :: String -> Bool
isLexicalRule [] = False
isLexicalRule (c:_) = isLower c

}
