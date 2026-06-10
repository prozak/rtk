{
module Parser where

import qualified Lexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
import Data.Generics
import Data.Data
import Data.Char
import Data.List (intercalate)
import qualified Data.Map as M
import qualified Data.Set as S

}

%name parse
%tokentype  { L.PosToken }
%error      { parseError }

%token

grammar { L.PosToken _ L.Grammar }
imports { L.PosToken _ L.Imports }
'='    { L.PosToken _ L.Eq }
'|'     { L.PosToken _ L.OrClause }
':'     { L.PosToken _ L.Colon }
';'     { L.PosToken _ L.RlEnd }
'*'     { L.PosToken _ L.Star }
'+'     { L.PosToken _ L.Plus }
'?'     { L.PosToken _ L.Question }
')'     { L.PosToken _ L.RParen }
'('     { L.PosToken _ L.LParen }
'.'     { L.PosToken _ L.Dot }
'!'     { L.PosToken _ L.Excl }
'~'     { L.PosToken _ L.Tilde }
','     { L.PosToken _ L.Comma }
'@shortcuts' { L.PosToken _ L.Shortcuts }
'@symmacro' { L.PosToken _ L.Symmacro }
id  { L.PosToken _ (L.Id _) }
str       { L.PosToken _ (L.StrLit $$) }
rexplit       { L.PosToken _ (L.RegExpLit $$) }
bigstr     { L.PosToken _ (L.BigStr $$) }
eof     { L.PosToken _ L.EndOfFile }

%%

Grammar : grammar str ';' ImportsOpt Rules eof { InitialGrammar $2 $4 (reverse $5) }

ImportsOpt : imports bigstr    { $2 }
           | {- empty -}       { "" }

Rules : RuleWithOptions                    { [$1] }
      | Rules RuleWithOptions              { $2 : $1 }
      | {- empty -}                        { [] }


RuleWithOptions : OptionsList Rule   { addRuleOptions (reverse $1) $2 }

OptionsList : OptionsList Option    { $2 : $1 }
            | {- empty -}           { [] }

Option : '@shortcuts' '(' IdListOpt ')'     { OShortcuts (reverse $3)}
       | '@symmacro'                        { OSymmacro }

IdListOpt : IdList                  { $1 }
          | {- empty -}             { [] } 

IdList : IdList ',' id              { idStr $3 : $1}
       | id                         { [idStr $1] }

Rule : id '=' ClauseAlt ';'         { IRule Nothing Nothing (idStr $1) $3 [] (Just (idPos $1)) }
     | id ':' id '=' ClauseAlt ';'  { IRule (Just (idStr $1)) Nothing (idStr $3) $5 [] (Just (idPos $1)) }
     | id '.' id ':' id '=' ClauseAlt ';'  { IRule (Just (idStr $1)) (Just (idStr $3)) (idStr $5) $7 [] (Just (idPos $1)) }
     | '.' id ':' id '=' ClauseAlt ';'  { IRule Nothing (Just (idStr $2)) (idStr $4) $6 [] (Just (idPos $2)) }

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
           | id                       { IId (idStr $1) }
           | str                      { IStrLit $1 }
           | '.'                      { IDot }
           | rexplit                  { IRegExpLit $1 }

OptDelim : {- empty -}          { Nothing }
         | '~' ClauseItem       { Just $2 }

{

parseError :: [L.PosToken] -> a
parseError [] = errorWithoutStackTrace "Parse error: unexpected end of input. Expected a grammar definition."
parseError (L.PosToken pos tok : rest) =
    errorWithoutStackTrace $ "Parse error at " ++ showAlexPos pos ++ ": unexpected " ++ showToken tok ++ following
  where following = case rest of
                      [] -> ""
                      _  -> ", followed by: " ++ intercalate ", " (map (showToken . L.ptToken) (take 4 rest))

showAlexPos :: L.AlexPosn -> String
showAlexPos (L.AlexPn _ line col) = "line " ++ show line ++ ", column " ++ show col

-- Render a token the way it appears in the grammar source, for error messages
showToken :: L.Token -> String
showToken L.Grammar        = "keyword 'grammar'"
showToken L.Imports        = "keyword 'imports'"
showToken L.Eq             = "'='"
showToken L.RlEnd          = "';'"
showToken L.OrClause       = "'|'"
showToken L.Dot            = "'.'"
showToken (L.RegExpLit s)  = "regular expression [" ++ s ++ "]"
showToken (L.StrLit s)     = "string literal '" ++ s ++ "'"
showToken (L.BigStr _)     = "multi-line string"
showToken (L.Id s)         = "identifier '" ++ s ++ "'"
showToken L.Star           = "'*'"
showToken L.Plus           = "'+'"
showToken L.Excl           = "'!'"
showToken L.Comma          = "','"
showToken L.RParen         = "')'"
showToken L.LParen         = "'('"
showToken L.Dollar         = "'$'"
showToken L.Question       = "'?'"
showToken L.Colon          = "':'"
showToken L.Tilde          = "'~'"
showToken L.Shortcuts      = "'@shortcuts'"
showToken L.Symmacro       = "'@symmacro'"
showToken L.EndOfFile      = "end of input"

idStr :: L.PosToken -> String
idStr (L.PosToken _ (L.Id s)) = s
idStr t = error $ "Internal error: identifier token expected, but got: " ++ show t

idPos :: L.PosToken -> SourcePos
idPos (L.PosToken (L.AlexPn _ line col) _) = SourcePos line col

-- Position in the grammar source file (line and column, both 1-based)
data SourcePos = SourcePos { srcLine :: Int, srcColumn :: Int }
                 deriving (Eq, Ord, Show, Typeable, Data)

showSourcePos :: SourcePos -> String
showSourcePos (SourcePos line col) = "line " ++ show line ++ ", column " ++ show col

data InitialGrammar = InitialGrammar { getIGrammarName :: String, getImports :: String, getIRules :: [IRule] }
                 deriving (Eq, Show, Typeable, Data)

data IRule = IRule { getIDataTypeName :: (Maybe String),
                     getIDataFunc :: (Maybe String),
                     getIRuleName :: String,
                     getIClause :: IClause,
                     getIRuleOptions :: [IOption],
                     getIRulePos :: (Maybe SourcePos)}
                  deriving (Eq, Show, Typeable, Data)

data IOption = OShortcuts [ID] | OSymmacro
                  deriving (Eq, Show, Typeable, Data)

addRuleOptions :: [IOption] -> IRule -> IRule
addRuleOptions opts rule = rule{ getIRuleOptions = opts ++ (getIRuleOptions rule)}                        

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

-- Render a clause the way it appears in the grammar source, for error messages
showClause :: IClause -> String
showClause (IId name)      = name
showClause (IStrLit s)     = "'" ++ s ++ "'"
showClause IDot            = "."
showClause (IRegExpLit s)  = "[" ++ s ++ "]"
showClause (IStar c md)    = showClause c ++ "*" ++ showDelim md
showClause (IPlus c md)    = showClause c ++ "+" ++ showDelim md
showClause (IAlt cs)       = "(" ++ intercalate " | " (map showClause cs) ++ ")"
showClause (ISeq cs)       = unwords (map showClause cs)
showClause (IOpt c)        = showClause c ++ "?"
showClause (ILifted c)     = "," ++ showClause c
showClause (IIgnore c)     = "!" ++ showClause c

showDelim :: Maybe IClause -> String
showDelim = maybe "" (\d -> " ~ " ++ showClause d)

data GrammarInfo =
  GrammarInfo
  {
     getStartRuleName :: Maybe String,
     getRuleToStartInfo :: M.Map String String,
     getNameCounter :: Int,
     getProxyRules :: S.Set String
  }
  deriving (Eq, Show, Typeable, Data)

data AntiRule = AntiRule { arTypeName :: ID,
                           arQQName :: ID,
                           arConstr :: ID ,
                           arIsList :: Bool 
                         }
                     deriving (Eq, Show, Typeable, Data)

data NormalGrammar = NormalGrammar { getNGrammarName :: String, 
                                     getSyntaxRuleGroups :: [SyntaxRuleGroup], 
                                     getLexicalRules :: [LexicalRule],
                                     getAntiRules :: [AntiRule],
                                     getShortcuts :: [(String, String)],
                                     getNImports :: String,
                                     getGrammarInfo :: GrammarInfo }
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
                   | MacroRule { getLRuleName :: String, getLClause :: LClause}
                   deriving (Eq, Show, Typeable, Data)

type LClause = IClause

isLexicalRule :: String -> Bool
isLexicalRule [] = False
isLexicalRule (c:_) = isLower c

filterProxyRules proxyRules rules = filter (((flip S.notMember) proxyRules) . getSDataTypeName) rules
}
