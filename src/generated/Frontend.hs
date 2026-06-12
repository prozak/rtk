{-# LANGUAGE QuasiQuotes #-}
-- | The front-end surface of the pipeline. Since task 8c the pipeline
-- computes directly over the GENERATED AST ('GrammarParser''s types, compiled
-- from the @test/golden/grammar/@ snapshot): there is no separate
-- hand-written grammar representation anymore. This module hosts what both
-- front ends share:
--
--   * the default (self-hosted) entry points: 'parseWithGenerated' lexes and
--     parses a grammar source with the lexer\/parser RTK generated from
--     @test-grammars/grammar.pg@;
--
--   * 'cleanGrammarTokens', the ONE implementation of token-level cleanup.
--     Both lexers keep the full token text (quotes around @'str'@, brackets
--     around @[regex]@, triple quotes around @\"\"\"bigstr\"\"\"@, escape
--     pairs intact), and both front ends run this pass right after parsing:
--     it strips the literal delimiters and applies
--     'TokenProcessing.unBackQuote' to string and regex leaves. The escape
--     logic is not duplicated anywhere else.
--
--   * the accessors and spine flatteners the pipeline stages
--     ('StringLiterals', 'Normalize', the generators, 'Debug') use to
--     compute over @GP.Grammar@\/@GP.Rule@\/@GP.Clause@ values, including
--     position projection into the diagnostics' 'SourcePos'.
--
-- Every generated constructor (except the quasi-quotation-only @Anti_*@
-- ones) carries the source position of its first symbol in its first field,
-- so diagnostics can point at the offending clause, not just the rule
-- header. Positions are equality-transparent ('GP.RtkPos'), which is why the
-- AST-equality harness projects them explicitly via 'sourcePosOf'.
--
-- Clause-shape matches here and in the later pipeline stages are written
-- with rtk's own generated quasi-quoter where that reads better than the
-- constructor spelling (task 8d): a @[clause| ... |]@ pattern is the clause
-- shape in grammar syntax, with @$cl...@ metavariables binding sub-clauses
-- (the names resolve through grammar.pg's @\@shortcuts@ table, so they must
-- start with a declared shortcut such as @cl@, or use the explicit
-- @$Clause:name@ form) and every position wildcarded.
module Frontend
    ( -- * Parsing entry points (the generated front end)
      parseWithGenerated
    , scanTokensGenerated
      -- * Token-level cleanup shared by both front ends
    , cleanGrammarTokens
      -- * Grammar accessors
    , grammarName
    , grammarImports
    , grammarRules
    , mapGrammarRules
      -- * Rule accessors
    , ruleName
    , ruleTypeName
    , ruleFunc
    , ruleClause
    , ruleOptions
    , rulePos
      -- * Clause helpers
    , altElems
    , seqElems
    , nameText
    , strLitText
    , showClause
    , showClauseSeq
      -- * Positions
    , sourcePosOf
    , clausePos
    , namePos
    ) where

import Data.Generics (everywhere, extT, mkT)
import Data.List (intercalate)

import qualified GrammarLexer as GL
import qualified GrammarParser as GP
import GrammarQQ (clause)

import Diagnostics (Diagnostic, SourcePos(..), diagnosticFromPositioned)
import TokenProcessing (unBackQuote)

-- | Lex and parse a grammar source with the generated front end and clean the
-- token text. The generated lexer and parser encode error positions as
-- @\"LINE:COL:message\"@ (the same encoding the hand-written lexer uses), so
-- their failures are split back into a positioned 'Diagnostic' here and
-- render with the same GNU-style @FILE:LINE:COL:@ prefix as the hand-written
-- front end's.
parseWithGenerated :: String -> Either Diagnostic GP.Grammar
parseWithGenerated src =
    case GL.scanTokens src >>= GP.parseGrammar of
        Left msg -> Left (diagnosticFromPositioned msg)
        Right g  -> Right (cleanGrammarTokens g)

-- | The generated lexer's token stream, rendered one token per line for
-- @--debug-tokens@. Token text is the raw match ('cleanGrammarTokens' runs
-- on the AST after parsing), so this is the stream exactly as the parser
-- consumes it.
scanTokensGenerated :: String -> Either Diagnostic [String]
scanTokensGenerated src =
    case GL.scanTokens src of
        Left msg   -> Left (diagnosticFromPositioned msg)
        Right toks -> Right (map show toks)

-- | Strip literal delimiters and process escapes on every token-text leaf of
-- a parsed grammar: quotes around string literals, brackets around regex
-- literals (both with 'unBackQuote' applied to the content) and the triple
-- quotes around the @imports@ block. Both front ends apply this right after
-- parsing; everything downstream sees clean text.
cleanGrammarTokens :: GP.Grammar -> GP.Grammar
cleanGrammarTokens = everywhere (mkT cleanStr `extT` cleanClause `extT` cleanTop)
  where
    cleanStr (GP.Str p s)  = GP.Str p (unBackQuote (stripEnds 1 s))
    cleanStr s             = s
    cleanClause (GP.Regex p s) = GP.Regex p (unBackQuote (stripEnds 1 s))
    cleanClause c              = c
    cleanTop (GP.GrammarImports p n imp rs) = GP.GrammarImports p n (stripEnds 3 imp) rs
    cleanTop g                              = g

-- | Drop @n@ delimiter characters from both ends of a token's text.
stripEnds :: Int -> String -> String
stripEnds n s = take (length s - 2 * n) (drop n s)

--------------------------------------------------------------------------------
-- Grammar accessors
--------------------------------------------------------------------------------

grammarName :: GP.Grammar -> String
grammarName (GP.GrammarDef _ n _)       = strLitText n
grammarName (GP.GrammarImports _ n _ _) = strLitText n
grammarName g                           = qqOnly "Grammar" g

grammarImports :: GP.Grammar -> String
grammarImports (GP.GrammarImports _ _ imp _) = imp
grammarImports (GP.GrammarDef _ _ _)         = ""
grammarImports g                             = qqOnly "Grammar" g

grammarRules :: GP.Grammar -> [GP.Rule]
grammarRules (GP.GrammarDef _ _ rs)       = rs
grammarRules (GP.GrammarImports _ _ _ rs) = rs
grammarRules g                            = qqOnly "Grammar" g

mapGrammarRules :: ([GP.Rule] -> [GP.Rule]) -> GP.Grammar -> GP.Grammar
mapGrammarRules f (GP.GrammarDef p n rs)         = GP.GrammarDef p n (f rs)
mapGrammarRules f (GP.GrammarImports p n imp rs) = GP.GrammarImports p n imp (f rs)
mapGrammarRules _ g                              = qqOnly "Grammar" g

--------------------------------------------------------------------------------
-- Rule accessors (transparent over the RuleWithOptions wrapper)
--------------------------------------------------------------------------------

ruleName :: GP.Rule -> String
ruleName (GP.RuleSimple _ n _)        = nameText n
ruleName (GP.RuleTyped _ _ n _)       = nameText n
ruleName (GP.RuleTypedFunc _ _ _ n _) = nameText n
ruleName (GP.RuleFunc _ _ n _)        = nameText n
ruleName (GP.RuleWithOptions _ _ r)   = ruleName r
ruleName r                            = qqOnly "Rule" r

ruleTypeName :: GP.Rule -> Maybe String
ruleTypeName (GP.RuleSimple _ _ _)        = Nothing
ruleTypeName (GP.RuleTyped _ t _ _)       = Just (nameText t)
ruleTypeName (GP.RuleTypedFunc _ t _ _ _) = Just (nameText t)
ruleTypeName (GP.RuleFunc _ _ _ _)        = Nothing
ruleTypeName (GP.RuleWithOptions _ _ r)   = ruleTypeName r
ruleTypeName r                            = qqOnly "Rule" r

ruleFunc :: GP.Rule -> Maybe String
ruleFunc (GP.RuleSimple _ _ _)        = Nothing
ruleFunc (GP.RuleTyped _ _ _ _)       = Nothing
ruleFunc (GP.RuleTypedFunc _ _ f _ _) = Just (nameText f)
ruleFunc (GP.RuleFunc _ f _ _)        = Just (nameText f)
ruleFunc (GP.RuleWithOptions _ _ r)   = ruleFunc r
ruleFunc r                            = qqOnly "Rule" r

ruleClause :: GP.Rule -> GP.Clause
ruleClause (GP.RuleSimple _ _ c)        = c
ruleClause (GP.RuleTyped _ _ _ c)       = c
ruleClause (GP.RuleTypedFunc _ _ _ _ c) = c
ruleClause (GP.RuleFunc _ _ _ c)        = c
ruleClause (GP.RuleWithOptions _ _ r)   = ruleClause r
ruleClause r                            = qqOnly "Rule" r

ruleOptions :: GP.Rule -> [GP.Option]
ruleOptions (GP.RuleWithOptions _ opts r) = opts ++ ruleOptions r
ruleOptions _                             = []

-- | A rule's position is where the rule header starts (for the @.Func:@ form
-- that is the dot itself), skipping the options wrapper - the same position
-- the pipeline reported back when it tracked rule positions explicitly.
rulePos :: GP.Rule -> Maybe SourcePos
rulePos (GP.RuleWithOptions _ _ r) = rulePos r
rulePos r                          = sourcePosOf (GP.rtkPosOf r)

--------------------------------------------------------------------------------
-- Clause helpers
--------------------------------------------------------------------------------

-- | Flatten the left-recursive @'|'@ spine into source-order alternatives.
-- The right operand of each node is a single alternative, never another
-- alternation (parenthesized alternations stay as elements).
altElems :: GP.Clause -> [GP.Clause]
altElems [clause| $cl1 | $cl2 |] = altElems cl1 ++ [cl2]
altElems c                       = [c]

-- | Flatten the left-recursive juxtaposition spine of one alternative.
seqElems :: GP.Clause -> [GP.Clause]
seqElems [clause| $cl1 $cl2 |] = seqElems cl1 ++ [cl2]
seqElems c                     = [c]

nameText :: GP.Name -> String
nameText (GP.Ident _ s)   = s
nameText n@GP.Anti_Name{} = qqOnly "Name" n

strLitText :: GP.StrLit -> String
strLitText (GP.Str _ s)       = s
strLitText s@GP.Anti_StrLit{} = qqOnly "StrLit" s

-- Render a clause the way it appears in the grammar source, for error messages
showClause :: GP.Clause -> String
showClause (GP.Ref _ n)         = nameText n
showClause (GP.Lit _ s)         = "'" ++ strLitText s ++ "'"
showClause (GP.Dot _)           = "."
showClause (GP.Regex _ s)       = "[" ++ s ++ "]"
showClause (GP.Star _ c)        = showClause c ++ "*"
showClause (GP.StarDelim _ c d) = showClause c ++ "* ~ " ++ showClause d
showClause (GP.Plus _ c)        = showClause c ++ "+"
showClause (GP.PlusDelim _ c d) = showClause c ++ "+ ~ " ++ showClause d
showClause (GP.Opt _ c)         = showClause c ++ "?"
showClause (GP.Lifted _ c)      = "," ++ showClause c
showClause (GP.Ignored _ c)     = "!" ++ showClause c
showClause (GP.Labeled _ n c)   = nameText n ++ ": " ++ showClause c
showClause c@GP.Alt{}           = "(" ++ intercalate " | " (map showClause (altElems c)) ++ ")"
showClause c@GP.Seq{}           = showClauseSeq (seqElems c)
showClause (GP.Anti_Clause v)   = "$" ++ v

-- | Render one alternative's element list (a flattened sequence).
showClauseSeq :: [GP.Clause] -> String
showClauseSeq = unwords . map showClause

--------------------------------------------------------------------------------
-- Positions
--------------------------------------------------------------------------------

-- | Project a generated-AST position into the diagnostics' 'SourcePos'.
-- 'GP.rtkNoPos' (synthesized nodes, @Anti_*@ splices) projects to 'Nothing'.
sourcePosOf :: GP.RtkPos -> Maybe SourcePos
sourcePosOf (GP.RtkPos (GL.AlexPn _ line col))
    | line <= 0 = Nothing
    | otherwise = Just (SourcePos line col)

clausePos :: GP.Clause -> Maybe SourcePos
clausePos = sourcePosOf . GP.rtkPosOf

namePos :: GP.Name -> Maybe SourcePos
namePos = sourcePosOf . GP.rtkPosOf

-- | The generated grammar also contains constructors that only quasi-quote
-- splices can produce: @Anti_*@ metavariables and the start-rule wrappers
-- around dummy tokens. Parsing a grammar source file cannot reach them, so
-- they are an internal error rather than a diagnostic.
qqOnly :: Show a => String -> a -> b
qqOnly ty v = error $ "Frontend: quasi-quotation-only " ++ ty
                   ++ " constructor cannot come from a grammar file: " ++ show v
