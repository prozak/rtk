-- | Helpers shared by the unit and golden test suites: the in-process
-- generation pipeline (the same stages app/main.hs runs) and locale-independent
-- file IO for grammar sources and generated artifacts.
module TestSupport
    ( parseGrammarSource
    , parseGrammarSourceGenerated
    , normalizeGrammarSource
    , normalizeParsedGrammar
    , artifactsFor
    , frontEndDivergentGrammars
    , grammarsDir
    , discoverGrammarFiles
    , readFileUtf8
    , writeFileUtf8
    ) where

import Control.Exception (evaluate)
import Data.List (sort)
import System.Directory (listDirectory)
import System.FilePath ((</>), takeExtension)
import System.IO

import ASTAdapter (parseWithGenerated)
import Diagnostics (Diagnostic)
import GenQ (genQ)
import GenX (genX)
import GenY (genY)
import Lexer (scanTokens)
import Normalize (fillConstructorNames, normalizeTopLevelClauses)
import Parser
import StringLiterals (normalizeStringLiterals)
import TokenProcessing (processTokens)

-- | Lexing, token post-processing and parsing of a grammar specification.
parseGrammarSource :: String -> Either Diagnostic InitialGrammar
parseGrammarSource src = scanTokens src >>= (parse . processTokens)

-- | The self-hosted front end: the same job as 'parseGrammarSource', done by
-- the lexer/parser RTK generated from grammar.pg plus the AST adapter.
parseGrammarSourceGenerated :: String -> Either Diagnostic InitialGrammar
parseGrammarSourceGenerated = parseWithGenerated

-- | The shared back half of the front-end pipeline: normalization of an
-- already-parsed grammar down to what the code generators consume.
normalizeParsedGrammar :: InitialGrammar -> Either Diagnostic NormalGrammar
normalizeParsedGrammar ig = do
    ng <- normalizeTopLevelClauses (normalizeStringLiterals ig)
    return (fillConstructorNames ng)

-- | The full front-end pipeline, producing the normalized grammar that the
-- code generators consume.
normalizeGrammarSource :: String -> Either Diagnostic NormalGrammar
normalizeGrammarSource src = parseGrammarSource src >>= normalizeParsedGrammar

-- | The three files rtk writes for a grammar, as (file name, content) pairs.
artifactsFor :: NormalGrammar -> Either Diagnostic [(FilePath, String)]
artifactsFor g = do
    x <- genX g
    y <- genY g
    q <- genQ g
    return [ (name ++ "Lexer.x",  x)
           , (name ++ "Parser.y", y)
           , (name ++ "QQ.hs",    q)
           ]
    where name = getNGrammarName g

-- | Grammars whose hand-written-front-end parse the generated front end
-- provably cannot reproduce, with the reason. These are differences between
-- the two definitions of the grammar language, not adapter bugs:
--
--   * The hand-written parser accepts an EMPTY ALTERNATIVE (haskell.pg has
--     @Gd = | ExpI ;@, parsed as @IAlt [ISeq [], …]@), a construct that
--     grammar.pg's own clause syntax cannot derive (its Clause2 requires at
--     least one Clause3), so the generated parser rejects the file.
--
--   * The hand-written parser keeps REDUNDANT PARENTHESES as semantic
--     grouping (java.pg has @(ImportStatement)*@, t1.pg has @(A B) C@ —
--     nested @IAlt [ISeq …]@ groups that normalize into extra proxy
--     sub-rules), while grammar.pg's @Clause5 = '(' ,Clause ')'@ lifts the
--     group, so the parens are simply absent from the generated AST and the
--     distinction cannot be recovered by the adapter.
--
-- The golden suite checks these grammars against the snapshots with the
-- hand-written front end only, and both suites fail as soon as a pinned
-- grammar stops diverging so the pin gets dropped. Resolving them means
-- deciding which front end defines the language — follow-up work tracked in
-- BOOTSTRAP.md ("Known divergences"), out of scope for the 7a milestone.
frontEndDivergentGrammars :: [(String, String)]
frontEndDivergentGrammars =
    [ ("haskell", "the empty alternative 'Gd = | ExpI ;' is hand-written-parser-only syntax")
    , ("java",    "redundant parens like '(ImportStatement)*' are grouping to the hand-written parser only")
    , ("t1",      "redundant parens like '(A B) C' are grouping to the hand-written parser only")
    ]

grammarsDir :: FilePath
grammarsDir = "test-grammars"

-- | All grammar specifications directly under the given directory, sorted.
discoverGrammarFiles :: FilePath -> IO [FilePath]
discoverGrammarFiles dir = do
    entries <- listDirectory dir
    return [ dir </> e | e <- sort entries, takeExtension e == ".pg" ]

-- Grammar files and generated artifacts are UTF-8. Use an explicit encoding
-- and newline mode so the tests do not depend on the ambient locale.
readFileUtf8 :: FilePath -> IO String
readFileUtf8 path = do
    h <- openFile path ReadMode
    hSetEncoding h utf8
    hSetNewlineMode h noNewlineTranslation
    contents <- hGetContents h
    _ <- evaluate (length contents)
    hClose h
    return contents

writeFileUtf8 :: FilePath -> String -> IO ()
writeFileUtf8 path contents = withFile path WriteMode $ \h -> do
    hSetEncoding h utf8
    hSetNewlineMode h noNewlineTranslation
    hPutStr h contents
