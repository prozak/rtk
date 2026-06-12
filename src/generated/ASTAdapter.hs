-- | The self-hosted front end: parse a grammar source with the lexer and
-- parser that RTK generated from @test-grammars/grammar.pg@, and convert the
-- resulting generated AST to the hand-written 'InitialGrammar' that the rest
-- of the pipeline consumes.
--
-- The generated modules ('GrammarLexer', 'GrammarParser') are compiled
-- straight from the checked-in golden snapshot in @test/golden/grammar/@, so
-- @make accept-golden@ keeps this front end in sync with the generators by
-- construction.
--
-- The conversion is plain total pattern matching on the generated
-- constructors, which carry the names grammar.pg assigns with its
-- alternative labels (@RuleSimple@, @Star@, @Labeled@, ...). It has to
-- replicate two things the hand-written front end does between lexing and
-- parsing:
--
--   * the hand-written lexer strips literal delimiters from token text
--     (quotes around @'str'@, brackets around @[regex]@, triple quotes around
--     @\"\"\"bigstr\"\"\"@) while the generated lexer keeps the full match,
--     so the adapter strips them here;
--
--   * 'TokenProcessing.processTokens' applies 'unBackQuote' to string and
--     regex literals, so the adapter applies the same exported function to
--     those leaves.
--
-- Every generated constructor (except the quasi-quotation-only @Anti_*@
-- ones) carries the source position of its first symbol in its first field;
-- the adapter maps the rule constructors' position into 'getIRulePos', so
-- diagnostics under the default front end point at real source locations,
-- the same ones the hand-written reference front end records.
module ASTAdapter
    ( parseWithGenerated
    , scanTokensGenerated
    , convertGrammar
    ) where

import qualified GrammarLexer as GL
import qualified GrammarParser as GP

import Diagnostics (Diagnostic, SourcePos(..), diagnosticFromPositioned)
import Syntax (IClause(..), IOption(..), IRule(..), InitialGrammar(..),
               addRuleOptions)
import TokenProcessing (unBackQuote)

-- | Lex and parse a grammar source with the generated front end and adapt the
-- result. The generated lexer and parser encode error positions as
-- @\"LINE:COL:message\"@ (the same encoding the hand-written lexer uses), so
-- their failures are split back into a positioned 'Diagnostic' here and
-- render with the same GNU-style @FILE:LINE:COL:@ prefix as the hand-written
-- front end's.
parseWithGenerated :: String -> Either Diagnostic InitialGrammar
parseWithGenerated src =
    case GL.scanTokens src >>= GP.parseGrammar of
        Left msg -> Left (diagnosticFromPositioned msg)
        Right g  -> Right (convertGrammar g)

-- | The generated lexer's token stream, rendered one token per line for
-- @--debug-tokens@. The generated front end has no separate token
-- post-processing stage (the adapter strips delimiters and escapes during
-- AST conversion), so this is the stream exactly as the parser consumes it.
scanTokensGenerated :: String -> Either Diagnostic [String]
scanTokensGenerated src =
    case GL.scanTokens src of
        Left msg   -> Left (diagnosticFromPositioned msg)
        Right toks -> Right (map show toks)

-- | Convert the generated AST to the hand-written 'InitialGrammar'.
convertGrammar :: GP.Grammar -> InitialGrammar
convertGrammar (GP.GrammarDef _ gname rules) =
    InitialGrammar { getIGrammarName = strLit gname
                   , getImports     = ""
                   , getIRules      = map rule rules
                   }
convertGrammar (GP.GrammarImports _ gname imports rules) =
    InitialGrammar { getIGrammarName = strLit gname
                   , getImports     = stripEnds 3 imports
                   , getIRules      = map rule rules
                   }
convertGrammar g = qqOnly "Grammar" g

rule :: GP.Rule -> IRule
rule (GP.RuleSimple p n c)         = mkRule p Nothing         Nothing         n c
rule (GP.RuleTyped p t n c)        = mkRule p (Just (name t)) Nothing         n c
rule (GP.RuleTypedFunc p t f n c)  = mkRule p (Just (name t)) (Just (name f)) n c
-- a rule's position is where the rule starts: for the '.Func:' form that is
-- the dot itself, matching the first-symbol positions captured by generated
-- parsers
rule (GP.RuleFunc p f n c)         = mkRule p Nothing         (Just (name f)) n c
rule (GP.RuleWithOptions _ opts r) = addRuleOptions (map option opts) (rule r)
rule r@GP.Anti_Rule{}              = qqOnly "Rule" r

mkRule :: GP.RtkPos -> Maybe String -> Maybe String -> GP.Name -> GP.Clause -> IRule
mkRule p dataType dataFunc n c =
    IRule dataType dataFunc (name n) (topClause c) [] (Just (sourcePos p))

-- | The position of a rule's first token, as the hand-written parser's
-- 'SourcePos' (the hand-written front end records the same token's position).
sourcePos :: GP.RtkPos -> SourcePos
sourcePos (GP.RtkPos (GL.AlexPn _ line col)) = SourcePos line col

option :: GP.Option -> IOption
option (GP.Shortcuts _ ids) = OShortcuts (map name ids)
option (GP.Symmacro _)      = OSymmacro
option o@GP.Anti_Option{}   = qqOnly "Option" o

-- | A clause in alternative position: a rule's right-hand side or a
-- parenthesized group. The hand-written parser always wraps these as an
-- alternative of sequences, even degenerate ones, so this does too.
topClause :: GP.Clause -> IClause
topClause c = IAlt (map altClause (altElems c))

-- | One alternative: a constructor label wraps the alternative's sequence
-- in 'ICtor', exactly like the hand-written parser's ClauseSeqL rule.
altClause :: GP.Clause -> IClause
altClause (GP.Labeled _ n body) = ICtor (name n) (ISeq (map preClause (seqElems body)))
altClause alt                   = ISeq (map preClause (seqElems alt))

-- | Flatten the left-recursive @'|'@ spine into source-order alternatives.
-- The right operand of each node is a single alternative, never another
-- alternation (parenthesized alternations stay as elements and get wrapped
-- by 'itemClause').
altElems :: GP.Clause -> [GP.Clause]
altElems (GP.Alt _ l r) = altElems l ++ [r]
altElems c              = [c]

-- | Flatten the left-recursive juxtaposition spine of one alternative.
seqElems :: GP.Clause -> [GP.Clause]
seqElems (GP.Seq _ l r) = seqElems l ++ [r]
seqElems c              = [c]

preClause :: GP.Clause -> IClause
preClause (GP.Lifted _ c)  = ILifted (postClause c)
preClause (GP.Ignored _ c) = IIgnore (postClause c)
preClause c                = postClause c

postClause :: GP.Clause -> IClause
postClause (GP.Star _ c)        = IStar (itemClause c) Nothing
postClause (GP.StarDelim _ c d) = IStar (itemClause c) (Just (itemClause d))
postClause (GP.Plus _ c)        = IPlus (itemClause c) Nothing
postClause (GP.PlusDelim _ c d) = IPlus (itemClause c) (Just (itemClause d))
postClause (GP.Opt _ c)         = IOpt (itemClause c)
postClause c                    = itemClause c

-- | A clause in item position: a leaf, or — when it is still a sequence,
-- alternation, label, lift, ignore or repetition — a construct that can only
-- have come from a parenthesized group, which the hand-written parser
-- represents as a nested @IAlt [...]@. The generated parser drops the
-- parentheses themselves, so redundant parens around a single leaf normalize
-- away.
itemClause :: GP.Clause -> IClause
itemClause (GP.Ref _ n)       = IId (name n)
itemClause (GP.Lit _ s)       = IStrLit (strLit s)
itemClause (GP.Dot _)         = IDot
itemClause (GP.Regex _ s)     = IRegExpLit (unBackQuote (stripEnds 1 s))
itemClause c@GP.Anti_Clause{} = qqOnly "Clause" c
itemClause c                  = topClause c

name :: GP.Name -> String
name (GP.Ident _ s)   = s
name n@GP.Anti_Name{} = qqOnly "Name" n

strLit :: GP.StrLit -> String
strLit (GP.Str _ s)       = unBackQuote (stripEnds 1 s)
strLit s@GP.Anti_StrLit{} = qqOnly "StrLit" s

-- | Drop @n@ delimiter characters from both ends of a token's text.
stripEnds :: Int -> String -> String
stripEnds n s = take (length s - 2 * n) (drop n s)

-- | The generated grammar also contains constructors that only quasi-quote
-- splices can produce: @Anti_*@ metavariables and the start-rule wrappers
-- around dummy tokens (synthesized alternatives, so they keep generated
-- positional names). Parsing a grammar source file cannot reach them, so
-- they are an internal error rather than a diagnostic.
qqOnly :: Show a => String -> a -> b
qqOnly ty v = error $ "ASTAdapter: quasi-quotation-only " ++ ty
                   ++ " constructor cannot come from a grammar file: " ++ show v
