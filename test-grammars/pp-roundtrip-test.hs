-- Round-trip test for the generated pretty-printers (task 9).
--
-- For each opt-in grammar this parses a corpus of source fragments to an AST,
-- prints the AST with the generated pp<Type>, reparses the printed text, and
-- asserts the reparsed AST equals the original. This is the safety oracle for
-- the structural printer: 7b's RtkPos is position-transparent, so the
-- reparsed AST (different source positions) still compares == to the original
-- with no stripping. Under-parenthesization or a dropped token therefore
-- shows up here as a failing round-trip, not a silently wrong program.
--
-- Exits non-zero on any mismatch or parse failure.

import Control.Monad (forM)
import System.Exit (exitFailure)

import qualified PLexer       as PL
import qualified PParser      as PP
import qualified PPP          as PPp
import qualified SandboxLexer as SL
import qualified SandboxParser as SP
import qualified SandboxPP    as SPp
import qualified GrammarLexer  as GL
import qualified GrammarParser as GR
import qualified GrammarPP     as GPp
-- Block-mode (task 9b) printers: a small bracket grammar and the c-compiler
-- tutorial grammar. Layout is whitespace, so block output must reparse to the
-- same AST exactly as flat does - this proves layout never breaks correctness.
import qualified BlockLexer  as BL
import qualified BlockParser as BP
import qualified BlockPP     as BPp
import qualified CLexer      as CL
import qualified CParser     as CP
import qualified CPP         as CPp

-- | Parse, print, reparse, compare. Returns True on a clean round-trip.
roundTrip :: (Eq a, Show a)
          => String                       -- ^ grammar label
          -> (String -> Either String a)  -- ^ parser
          -> (a -> String)                -- ^ generated printer
          -> String                       -- ^ source fragment
          -> IO Bool
roundTrip label parse pp src =
    case parse src of
        Left err -> do
            putStrLn $ "[" ++ label ++ "] FAIL: input did not parse: " ++ show src
            putStrLn $ "  error: " ++ err
            return False
        Right ast ->
            let printed = pp ast in
            case parse printed of
                Left err -> do
                    putStrLn $ "[" ++ label ++ "] FAIL: printed text did not reparse"
                    putStrLn $ "  input:   " ++ show src
                    putStrLn $ "  printed: " ++ show printed
                    putStrLn $ "  error:   " ++ err
                    return False
                Right ast'
                    | ast == ast' -> do
                        putStrLn $ "[" ++ label ++ "] ok: " ++ show src
                        return True
                    | otherwise -> do
                        putStrLn $ "[" ++ label ++ "] FAIL: round-trip changed the AST"
                        putStrLn $ "  input:    " ++ show src
                        putStrLn $ "  printed:  " ++ show printed
                        putStrLn $ "  original: " ++ show ast
                        putStrLn $ "  reparsed: " ++ show ast'
                        return False

parseP :: String -> Either String PP.P
parseP src = PL.scanTokens src >>= PP.parseP

parseSandbox :: String -> Either String SP.Sandbox
parseSandbox src = SL.scanTokens src >>= SP.parseSandbox

parseGrammar :: String -> Either String GR.Grammar
parseGrammar src = GL.scanTokens src >>= GR.parseGrammar

parseBlock :: String -> Either String BP.Program
parseBlock src = BL.scanTokens src >>= BP.parseBlock

parseC :: String -> Either String CP.Program
parseC src = CL.scanTokens src >>= CP.parseC

pInputs :: [String]
pInputs =
    [ "(lambda (x) x)"
    , "(lambda (x) 0)"
    , "(lambda (x) (not x))"
    , "(lambda (x) (plus x 1))"
    , "(lambda (x) (if0 x 0 1))"
    , "(lambda (x) (and (shr16 x) (or x 1)))"
    , "(lambda (x) (fold x 0 (lambda (y z) (or y z))))"
    ]

-- Sandbox's only payload is the 'doccomment' token, stored verbatim and
-- emitted verbatim by the printer. The sandbox grammar lexes a content-bearing
-- comment as a doccomment only when it spans lines (a single-line "/** x */"
-- is shadowed by the ignored blockComment rule - a property of that grammar,
-- not of the printer), so the round-trip corpus uses multi-line comments.
sandboxInputs :: [String]
sandboxInputs =
    [ "/***/"
    , "/**\n*/"
    , "/**\n * a multi-line doc comment\n */"
    , "/**\n * doc with * stars and / slashes\n */"
    ]

-- Capstone (task 9): RTK prints its OWN grammar language. The printer RTK
-- generates for grammar.pg round-trips representative grammar ASTs - string
-- literals, regexes, typed/func/lifted rules, repetition/option/delimiter
-- forms and an imports block. (grammar.pg's own source does not fully
-- round-trip in v1: its str/regexplit/bigstr rules use repetition over an
-- un-parenthesized alternation, e.g. ([^\\'] | backslash .)*, which a
-- structural printer with no paren-insertion engine re-associates on reparse.
-- That under-parenthesization is exactly what this round-trip oracle is built
-- to catch; auto-parenthesization is a deliberately separate later task.)
grammarInputs :: [String]
grammarInputs =
    [ "grammar 'P';\nP = '(' 'lambda' '(' Id ')' E ')' ;\nId = id ;\nid = [a-z]+ ;\n"
    , "grammar 'G';\nA = Foo: Name '=' Clause ';' | ,B ;\nB = bar ;\nName = id ;\nInt: num = [0-9]+ ;\n"
    , "grammar 'G';\nA = B* ~ ',' ;\nB = C+ ;\nC = name? ;\nname = [a-z]+ ;\n"
    , "grammar 'G';\nimports \"\"\"\nimport Data.List\n\"\"\"\nA = a ;\na = [a]+ ;\n"
    ]

-- Block-mode inputs: bracket-structured programs with nested blocks. The
-- printer indents and line-breaks them, and they must reparse identically.
blockInputs :: [String]
blockInputs =
    [ "fn main ( ) { }"
    , "fn main ( a , b ) { x = 1 ; return x ; }"
    , "fn f ( ) { x = 1 ; { y = 2 ; return y ; } return x ; }"
    ]

cInputs :: [String]
cInputs =
    [ "int main ( ) { return 2 ; }"
    , "int main ( ) { return 2 ; return 3 ; return 42 ; }"
    ]

main :: IO ()
main = do
    pResults <- forM pInputs       $ roundTrip "P"            parseP       PPp.ppP
    sResults <- forM sandboxInputs $ roundTrip "Sandbox"      parseSandbox SPp.ppSandbox
    gResults <- forM grammarInputs $ roundTrip "Grammar"      parseGrammar GPp.ppGrammar
    bResults <- forM blockInputs   $ roundTrip "Block(block)" parseBlock   BPp.ppProgram
    cResults <- forM cInputs       $ roundTrip "C(block)"     parseC       CPp.ppProgram
    let results = pResults ++ sResults ++ gResults ++ bResults ++ cResults
    putStrLn ""
    if and results
        then putStrLn $ "PP round-trip tests: PASS (" ++ show (length results) ++ " fragments)"
        else do
            putStrLn $ "PP round-trip tests: FAIL ("
                       ++ show (length (filter not results)) ++ " of "
                       ++ show (length results) ++ " fragments)"
            exitFailure
