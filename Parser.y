{
module Parser where

import qualified Lexer as L (Token(..), PosToken(..), AlexPosn(..), alexScanTokens)
import Diagnostics (Diagnostic(..), SourcePos(..))
import Syntax
import Data.List (intercalate)

}

%name parse
%tokentype  { L.PosToken }
%monad      { Either Diagnostic }
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
     -- a rule's position is where the rule starts: for the '.' form that is
     -- the dot itself, matching the first-symbol positions captured by
     -- generated parsers
     | '.' id ':' id '=' ClauseAlt ';'  { IRule Nothing (Just (idStr $2)) (idStr $4) $6 [] (Just (idPos $1)) }

ClauseAlt : ClauseAlt1              { mkAlt (reverse $1) }

ClauseAlt1 : ClauseAlt1 '|' ClauseSeq   { $3 : $1 }
           | ClauseSeq                  { [$1] }

ClauseSeq : ClauseSeq1              { mkSeq (reverse $1) }

-- A sequence has at least one element: grammar.pg's clause syntax cannot
-- derive an empty alternative, and the reference parser must define the same
-- language
ClauseSeq1 : ClauseSeq1 ClausePre    { $2 : $1 }
           | ClausePre               { [$1] }

ClausePre :  ',' ClausePost           { ILifted (unliftGroupOp $2) }
           | '!' ClausePost           { IIgnore (unliftGroupOp $2) }
           | ClausePost               { unliftGroupElem $1 }

ClausePost : ClauseItem '*' OptDelim  { IStar $1 $3 }
           | ClauseItem '+' OptDelim  { IPlus $1 $3 }
           | ClauseItem '?'           { IOpt $1 }
           | ClauseItem               { $1 }


ClauseItem : '(' ClauseAlt ')'        { unliftGroupLeaf $2 }
           | id                       { IId (idStr $1) }
           | str                      { IStrLit $1 }
           | '.'                      { IDot }
           | rexplit                  { IRegExpLit $1 }

OptDelim : {- empty -}          { Nothing }
         | '~' ClauseItem       { Just $2 }

{

-- grammar.pg, the authoritative definition of the grammar language, treats
-- parentheses as pure grouping: its 'Clause5 = '(' ,Clause ')'' lifts the
-- group, so the generated front end records no node for the parentheses
-- themselves. The reference parser mirrors that with the helpers below, which
-- reproduce exactly what the lifted parse trees look like after
-- ASTAdapter flattens them (the AST-equality test suite holds the two front
-- ends to it for every corpus grammar):
--
--   * a group around a single leaf collapses to the leaf (every position);
--   * in sequence-element position, a group around a single repetition,
--     option, lifted or ignored clause collapses to that clause;
--   * a pure-sequence group at the head of a sequence merges into it (the
--     juxtaposition spine is left-recursive, so only the head merges);
--   * an alternation group that is the whole first alternative merges into
--     the enclosing alternation (the '|' spine is left-recursive too).

-- Group content that collapses everywhere parentheses themselves may appear
isLeafClause :: IClause -> Bool
isLeafClause (IId _)        = True
isLeafClause (IStrLit _)    = True
isLeafClause IDot           = True
isLeafClause (IRegExpLit _) = True
isLeafClause _              = False

-- In item position (operand of '*'/'+'/'?' and the '~' delimiter) only a
-- single-leaf group collapses
unliftGroupLeaf :: IClause -> IClause
unliftGroupLeaf (IAlt [ISeq [c]]) | isLeafClause c = c
unliftGroupLeaf c = c

-- Under ',' and '!' a group around one repetition or option collapses too
unliftGroupOp :: IClause -> IClause
unliftGroupOp (IAlt [ISeq [c]]) | isOpClause c = c
  where isOpClause IStar{} = True
        isOpClause IPlus{} = True
        isOpClause IOpt{}  = True
        isOpClause _       = False
unliftGroupOp c = c

-- In sequence-element position lifted and ignored content collapses as well
unliftGroupElem :: IClause -> IClause
unliftGroupElem (IAlt [ISeq [c]]) | isElemClause c = c
  where isElemClause IStar{}   = True
        isElemClause IPlus{}   = True
        isElemClause IOpt{}    = True
        isElemClause ILifted{} = True
        isElemClause IIgnore{} = True
        isElemClause _         = False
unliftGroupElem c = c

-- A pure-sequence group at the head of a sequence merges into it
mkSeq :: [IClause] -> IClause
mkSeq (IAlt [ISeq xs] : rest) = ISeq (xs ++ rest)
mkSeq cs                      = ISeq cs

-- An alternation group as the whole first alternative merges into the
-- enclosing alternation (a single-alternative group cannot reach here: mkSeq
-- has already merged it into its alternative's sequence)
mkAlt :: [IClause] -> IClause
mkAlt (ISeq [IAlt alts] : rest) = IAlt (alts ++ rest)
mkAlt alts                      = IAlt alts

parseError :: [L.PosToken] -> Either Diagnostic a
parseError [] = Left $ Diagnostic Nothing Nothing "unexpected end of input; expected a grammar definition"
parseError (L.PosToken pos tok : rest) =
    Left $ Diagnostic (Just (alexPosToSourcePos pos)) Nothing
                      ("unexpected " ++ showToken tok ++ following)
  where following = case rest of
                      [] -> ""
                      _  -> ", followed by: " ++ intercalate ", " (map (showToken . L.ptToken) (take 4 rest))

alexPosToSourcePos :: L.AlexPosn -> SourcePos
alexPosToSourcePos (L.AlexPn _ line col) = SourcePos line col

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
}
