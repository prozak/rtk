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

import qualified GrammarParser as GP

import Diagnostics (Diagnostic)
import Frontend (cleanGrammarTokens, parseWithGenerated)
import GenPP (genPP, PPLayout(..))
import GenQ (genQ)
import GenX (genX)
import GenY (genY)
import Lexer (scanTokens)
import Normalize (fillConstructorNames, normalizeTopLevelClauses)
import Parser (parse)
import StringLiterals (normalizeStringLiterals)
import Syntax
import TokenProcessing (processTokens)

-- | The hand-written reference front end: lexing, token post-processing,
-- parsing and the shared token-text cleanup. Both front ends produce the
-- GENERATED AST ('GP.Grammar'); the pipeline has no other parsed-grammar
-- representation.
parseGrammarSource :: String -> Either Diagnostic GP.Grammar
parseGrammarSource src = cleanGrammarTokens <$> (scanTokens src >>= (parse . processTokens))

-- | The self-hosted front end (the default): the same job as
-- 'parseGrammarSource', done by the lexer/parser RTK generated from
-- grammar.pg.
parseGrammarSourceGenerated :: String -> Either Diagnostic GP.Grammar
parseGrammarSourceGenerated = parseWithGenerated

-- | The shared back half of the front-end pipeline: normalization of an
-- already-parsed grammar down to what the code generators consume.
normalizeParsedGrammar :: GP.Grammar -> Either Diagnostic NormalGrammar
normalizeParsedGrammar pg = do
    ng <- normalizeTopLevelClauses (normalizeStringLiterals pg)
    return (fillConstructorNames ng)

-- | The full front-end pipeline, producing the normalized grammar that the
-- code generators consume.
normalizeGrammarSource :: String -> Either Diagnostic NormalGrammar
normalizeGrammarSource src = parseGrammarSource src >>= normalizeParsedGrammar

-- | Grammars (by 'getNGrammarName') that opt in to a pretty-printer golden,
-- with the layout to pin. The PP generator is opt-in (task 9): only these
-- grammars get a @\<Name\>PP.hs@ snapshot, so every other grammar sees zero
-- golden churn. @P@/@Sandbox@ pin the flat layout (task 9a, byte-unchanged);
-- @Block@ pins the block layout (task 9b) over a small bracket-structured
-- grammar so the indented output is reviewable. Keep this small.
ppGoldenGrammars :: [(String, PPLayout)]
ppGoldenGrammars = [("P", PPFlat), ("Sandbox", PPFlat), ("Block", PPBlock)]

-- | The files rtk writes for a grammar, as (file name, content) pairs: the
-- lexer, parser and quasi-quoter for every grammar, plus the pretty-printer
-- (in its pinned layout) for the grammars opted in to 'ppGoldenGrammars'.
artifactsFor :: NormalGrammar -> Either Diagnostic [(FilePath, String)]
artifactsFor g = do
    x <- genX g
    y <- genY g
    q <- genQ g
    pp <- case lookup name ppGoldenGrammars of
            Just layout -> (\s -> [(name ++ "PP.hs", s)]) <$> genPP layout g
            Nothing     -> return []
    return $ [ (name ++ "Lexer.x",  x)
             , (name ++ "Parser.y", y)
             , (name ++ "QQ.hs",    q)
             ] ++ pp
    where name = getNGrammarName g

-- | Grammars whose hand-written-front-end parse the generated front end
-- provably cannot reproduce, with the reason.
--
-- EMPTY since the generated front end became the default: the two historic
-- divergences were resolved by making the reference parser define the same
-- language as grammar.pg (empty alternatives are rejected, redundant
-- parentheses are lifted exactly like grammar.pg's @Clause5 = '(' ,Clause ')'@
-- does) and by rewriting haskell.pg's @Gd = | ExpI ;@ as @Gd = ExpI? ;@. Every
-- grammar in the corpus now passes the strict dual-front-end equivalence.
--
-- Should a new divergence ever have to be tolerated temporarily, pin the
-- grammar here: the golden suite then checks it with the reference front end
-- only, and both suites fail as soon as the grammar stops diverging so the
-- pin gets dropped again.
frontEndDivergentGrammars :: [(String, String)]
frontEndDivergentGrammars = []

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
