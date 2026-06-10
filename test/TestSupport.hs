-- | Helpers shared by the unit and golden test suites: the in-process
-- generation pipeline (the same stages app/main.hs runs) and locale-independent
-- file IO for grammar sources and generated artifacts.
module TestSupport
    ( parseGrammarSource
    , normalizeGrammarSource
    , artifactsFor
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

-- | The full front-end pipeline, producing the normalized grammar that the
-- code generators consume.
normalizeGrammarSource :: String -> Either Diagnostic NormalGrammar
normalizeGrammarSource src = do
    ig <- parseGrammarSource src
    ng <- normalizeTopLevelClauses (normalizeStringLiterals ig)
    return (fillConstructorNames ng)

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
