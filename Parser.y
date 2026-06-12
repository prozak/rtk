{
module Parser where

import qualified Lexer as L (Token(..), PosToken(..), AlexPosn(..))
import qualified GrammarLexer as GL (AlexPosn(..))
import qualified GrammarParser as GP
import Diagnostics (Diagnostic(..), SourcePos(..))
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
str       { L.PosToken _ (L.StrLit _) }
rexplit       { L.PosToken _ (L.RegExpLit _) }
bigstr     { L.PosToken _ (L.BigStr _) }
eof     { L.PosToken _ L.EndOfFile }

%%

-- The reference parser builds the GENERATED AST (GrammarParser's types)
-- directly: same constructors, same binary Alt/Seq spines, same
-- first-symbol positions as the parser RTK generates from grammar.pg. The
-- AST-equality test suite holds the two front ends to it for every corpus
-- grammar, source positions included (projected explicitly, since RtkPos is
-- equality-transparent). Token text stays raw here too; the shared
-- Frontend.cleanGrammarTokens pass strips delimiters and escapes after
-- parsing.

Grammar : grammar str ';' Rules eof                { GP.GrammarDef (posOf $1) (mkStr $2) (reverse $4) }
        | grammar str ';' imports bigstr Rules eof { GP.GrammarImports (posOf $1) (mkStr $2) (bigStrText $5) (reverse $6) }

Rules : Rules RuleWithOptions              { $2 : $1 }
      | {- empty -}                        { [] }


RuleWithOptions : OptionsList Rule   { mkRule (reverse $1) $2 }

OptionsList : OptionsList Option    { $2 : $1 }
            | {- empty -}           { [] }

Option : '@shortcuts' '(' IdListOpt ')'     { GP.Shortcuts (posOf $1) $3 }
       | '@symmacro'                        { GP.Symmacro (posOf $1) }

IdListOpt : IdList                  { reverse $1 }
          | {- empty -}             { [] }

IdList : IdList ',' id              { mkName $3 : $1}
       | id                         { [mkName $1] }

Rule : id '=' ClauseAlt ';'         { GP.RuleSimple (posOf $1) (mkName $1) $3 }
     | id ':' id '=' ClauseAlt ';'  { GP.RuleTyped (posOf $1) (mkName $1) (mkName $3) $5 }
     | id '.' id ':' id '=' ClauseAlt ';'  { GP.RuleTypedFunc (posOf $1) (mkName $1) (mkName $3) (mkName $5) $7 }
     -- a rule's position is where the rule starts: for the '.' form that is
     -- the dot itself, matching the first-symbol positions captured by
     -- generated parsers
     | '.' id ':' id '=' ClauseAlt ';'  { GP.RuleFunc (posOf $1) (mkName $2) (mkName $4) $6 }

ClauseAlt : ClauseAlt '|' ClauseSeqL    { GP.Alt (GP.rtkPosOf $1) $1 $3 }
          | ClauseSeqL                  { $1 }

-- A named alternative: "Star: Clause5 '*'" names the alternative's AST
-- constructor. The label binds tighter than '|' and scopes over the whole
-- sequence, mirroring grammar.pg's Clause1 rule. No conflict with the
-- 'Type ':' Name '='' rule-header forms: ':' never follows a clause symbol
-- (clauses are fenced off by '=' ... ';'), so on ':' the parser always
-- shifts toward the label.
ClauseSeqL : id ':' ClauseSeq           { GP.Labeled (posOf $1) (mkName $1) $3 }
           | ClauseSeq                  { $1 }

-- A sequence has at least one element: grammar.pg's clause syntax cannot
-- derive an empty alternative, and the reference parser must define the same
-- language
ClauseSeq : ClauseSeq ClausePre      { GP.Seq (GP.rtkPosOf $1) $1 $2 }
          | ClausePre                { $1 }

ClausePre :  ',' ClausePost           { GP.Lifted (posOf $1) $2 }
           | '!' ClausePost           { GP.Ignored (posOf $1) $2 }
           | ClausePost               { $1 }

ClausePost : ClauseItem '*' OptDelim  { mkMany GP.Star GP.StarDelim $1 $3 }
           | ClauseItem '+' OptDelim  { mkMany GP.Plus GP.PlusDelim $1 $3 }
           | ClauseItem '?'           { GP.Opt (GP.rtkPosOf $1) $1 }
           | ClauseItem               { $1 }

-- Parentheses are pure grouping, exactly like grammar.pg's
-- 'Clause5 = '(' ,Clause ')'': the group content passes through and no node
-- records the parentheses themselves.
ClauseItem : '(' ClauseAlt ')'        { $2 }
           | id                       { GP.Ref (posOf $1) (mkName $1) }
           | str                      { GP.Lit (posOf $1) (mkStr $1) }
           | '.'                      { GP.Dot (posOf $1) }
           | rexplit                  { GP.Regex (posOf $1) (rexText $1) }

OptDelim : {- empty -}          { Nothing }
         | '~' ClauseItem       { Just $2 }

{

-- The generated AST's positions are GrammarLexer's AlexPosn wrapped in
-- RtkPos; rebuild them from the hand-written lexer's identical AlexPosn.
posOf :: L.PosToken -> GP.RtkPos
posOf (L.PosToken (L.AlexPn a line col) _) = GP.RtkPos (GL.AlexPn a line col)

mkName :: L.PosToken -> GP.Name
mkName t@(L.PosToken _ (L.Id s)) = GP.Ident (posOf t) s
mkName t = error $ "Internal error: identifier token expected, but got: " ++ show t

mkStr :: L.PosToken -> GP.StrLit
mkStr t@(L.PosToken _ (L.StrLit s)) = GP.Str (posOf t) s
mkStr t = error $ "Internal error: string literal token expected, but got: " ++ show t

rexText :: L.PosToken -> String
rexText (L.PosToken _ (L.RegExpLit s)) = s
rexText t = error $ "Internal error: regex literal token expected, but got: " ++ show t

bigStrText :: L.PosToken -> String
bigStrText (L.PosToken _ (L.BigStr s)) = s
bigStrText t = error $ "Internal error: big string token expected, but got: " ++ show t

-- Options wrap the rule only when present, like grammar.pg's
-- 'Rule = RuleWithOptions: OptionList Rule1 | ,Rule1' (OptionList is
-- non-empty there); the wrapper's position is the first option's.
mkRule :: [GP.Option] -> GP.Rule -> GP.Rule
mkRule []           r = r
mkRule opts@(o : _) r = GP.RuleWithOptions (GP.rtkPosOf o) opts r

-- A repetition node: plain or with a '~' delimiter, positioned at the
-- repeated clause's first symbol like the generated parser's actions.
mkMany :: (GP.RtkPos -> GP.Clause -> GP.Clause)
       -> (GP.RtkPos -> GP.Clause -> GP.Clause -> GP.Clause)
       -> GP.Clause -> Maybe GP.Clause -> GP.Clause
mkMany plain _     c Nothing  = plain (GP.rtkPosOf c) c
mkMany _ delimited c (Just d) = delimited (GP.rtkPosOf c) c d

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

-- Render a token the way it appears in the grammar source, for error
-- messages. Literal tokens carry their raw text (delimiters included), so
-- they render as-is.
showToken :: L.Token -> String
showToken L.Grammar        = "keyword 'grammar'"
showToken L.Imports        = "keyword 'imports'"
showToken L.Eq             = "'='"
showToken L.RlEnd          = "';'"
showToken L.OrClause       = "'|'"
showToken L.Dot            = "'.'"
showToken (L.RegExpLit s)  = "regular expression " ++ s
showToken (L.StrLit s)     = "string literal " ++ s
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
}
