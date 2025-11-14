{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import Text.Show.Pretty
import System.Exit(exitFailure, exitSuccess)

-- Full parsing test driver using RTK-generated lexer + parser
-- This tests the complete parsing pipeline with the RTK-generated Java grammar
main = do
    args <- getArgs
    case args of
        [file] -> do
            content <- readFile file
            let tokens = alexScanTokens content
            let tokenCount = length tokens  -- Force evaluation
            let ast = parseJava tokens

            putStrLn "=== Tokenization successful ==="
            putStrLn $ "Token count: " ++ show tokenCount

            putStrLn "\n=== Parsing successful ==="
            putStrLn $ ppShow ast

            putStrLn "\n=== Java grammar test PASSED ==="
            exitSuccess

        _ -> do
            putStrLn "Usage: java-main-full <java-file>"
            exitFailure
