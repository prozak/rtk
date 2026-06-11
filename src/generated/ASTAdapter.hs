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
-- constructors. It has to replicate two things the hand-written front end
-- does between lexing and parsing:
--
--   * the hand-written lexer strips literal delimiters from token text
--     (quotes around @'str'@, brackets around @[regex]@, triple quotes around
--     @\"\"\"bigstr\"\"\"@) while the generated lexer keeps the full match,
--     so 'convertGrammar' strips them here;
--
--   * 'TokenProcessing.processTokens' applies 'unBackQuote' to string and
--     regex literals, so 'convertGrammar' applies the same exported function
--     to those leaves.
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
convertGrammar (GP.Ctr__Grammar__11 _ gname imports rules) =
    InitialGrammar { getIGrammarName = strLit gname
                   , getImports     = importsOpt imports
                   , getIRules      = map rule rules
                   }
convertGrammar g = qqOnly "Grammar" g

rule :: GP.Rule -> IRule
rule (GP.Ctr__Rule__0 p n c)     = mkRule p Nothing         Nothing         n c
rule (GP.Ctr__Rule__1 p t n c)   = mkRule p (Just (name t)) Nothing         n c
rule (GP.Ctr__Rule__2 p t f n c) = mkRule p (Just (name t)) (Just (name f)) n c
rule (GP.Ctr__Rule__3 p f n c)   = mkRule p Nothing         (Just (name f)) n c
rule (GP.Ctr__Rule__4 _ opts r)  = addRuleOptions (map option opts) (rule r)
rule r@GP.Anti_Rule{}            = qqOnly "Rule" r

mkRule :: GP.RtkPos -> Maybe String -> Maybe String -> GP.Name -> GP.Clause -> IRule
mkRule p dataType dataFunc n c =
    IRule dataType dataFunc (name n) (topClause c) [] (Just (sourcePos p))

-- | The position of a rule's first token, as the hand-written parser's
-- 'SourcePos' (the hand-written front end records the same token's position).
sourcePos :: GP.RtkPos -> SourcePos
sourcePos (GP.RtkPos (GL.AlexPn _ line col)) = SourcePos line col

option :: GP.Option -> IOption
option (GP.Ctr__Option__0 _ ids) = OShortcuts (map name ids)
option (GP.Ctr__Option__1 _)     = OSymmacro
option o@GP.Anti_Option{}        = qqOnly "Option" o

importsOpt :: GP.ImportsOpt -> String
importsOpt (GP.Ctr__ImportsOpt__0 _)                           = ""
importsOpt (GP.Ctr__ImportsOpt__1 _ (GP.Ctr__Rule_0__0 _ big)) = stripEnds 3 big
importsOpt i@GP.Anti_ImportsOpt{}                              = qqOnly "ImportsOpt" i

-- | A clause in alternative position: a rule's right-hand side or a
-- parenthesized group. The hand-written parser always wraps these as an
-- alternative of sequences, even degenerate ones, so this does too.
topClause :: GP.Clause -> IClause
topClause c = IAlt [ ISeq (map preClause (seqElems alt)) | alt <- altElems c ]

-- | Flatten the left-recursive @'|'@ spine into source-order alternatives.
-- The right operand of each node is a single alternative, never another
-- alternation (parenthesized alternations stay as elements and get wrapped
-- by 'itemClause').
altElems :: GP.Clause -> [GP.Clause]
altElems (GP.Ctr__Clause__14 _ l r) = altElems l ++ [r]
altElems c                          = [c]

-- | Flatten the left-recursive juxtaposition spine of one alternative.
seqElems :: GP.Clause -> [GP.Clause]
seqElems (GP.Ctr__Clause__12 _ l r) = seqElems l ++ [r]
seqElems c                          = [c]

preClause :: GP.Clause -> IClause
preClause (GP.Ctr__Clause__9 _ c)  = ILifted (postClause c)
preClause (GP.Ctr__Clause__10 _ c) = IIgnore (postClause c)
preClause c                        = postClause c

postClause :: GP.Clause -> IClause
postClause (GP.Ctr__Clause__5 _ c d) = IStar (itemClause c) (delim d)
postClause (GP.Ctr__Clause__6 _ c d) = IPlus (itemClause c) (delim d)
postClause (GP.Ctr__Clause__7 _ c)   = IOpt (itemClause c)
postClause c                         = itemClause c

-- | A clause in item position: a leaf, or — when it is still a sequence,
-- alternation, lift, ignore or repetition — a construct that can only have
-- come from a parenthesized group, which the hand-written parser represents
-- as a nested @IAlt [ISeq …]@. The generated parser drops the parentheses
-- themselves, so redundant parens around a single leaf normalize away.
itemClause :: GP.Clause -> IClause
itemClause (GP.Ctr__Clause__1 _ n) = IId (name n)
itemClause (GP.Ctr__Clause__2 _ s) = IStrLit (strLit s)
itemClause (GP.Ctr__Clause__3 _)   = IDot
itemClause (GP.Ctr__Clause__4 _ s) = IRegExpLit (unBackQuote (stripEnds 1 s))
itemClause c@GP.Anti_Clause{}      = qqOnly "Clause" c
itemClause c                       = topClause c

delim :: GP.OptDelim -> Maybe IClause
delim (GP.Ctr__OptDelim__0 _)                            = Nothing
delim (GP.Ctr__OptDelim__1 _ (GP.Ctr__Rule_4__0 _ c))    = Just (itemClause c)
delim d@GP.Anti_OptDelim{}                               = qqOnly "OptDelim" d

name :: GP.Name -> String
name (GP.Ctr__Name__0 _ s) = s
name n@GP.Anti_Name{}      = qqOnly "Name" n

strLit :: GP.StrLit -> String
strLit (GP.Ctr__StrLit__0 _ s) = unBackQuote (stripEnds 1 s)
strLit s@GP.Anti_StrLit{}      = qqOnly "StrLit" s

-- | Drop @n@ delimiter characters from both ends of a token's text.
stripEnds :: Int -> String -> String
stripEnds n s = take (length s - 2 * n) (drop n s)

-- | The generated grammar also contains constructors that only quasi-quote
-- splices can produce: @Anti_*@ metavariables and the @Ctr__Grammar__0..10@
-- start-rule wrappers around dummy tokens. Parsing a grammar source file
-- cannot reach them, so they are an internal error rather than a diagnostic.
qqOnly :: Show a => String -> a -> b
qqOnly ty v = error $ "ASTAdapter: quasi-quotation-only " ++ ty
                   ++ " constructor cannot come from a grammar file: " ++ show v
