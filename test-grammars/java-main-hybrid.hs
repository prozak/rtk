{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import Text.Show.Pretty
import System.Exit(exitFailure, exitSuccess)

-- Hybrid test driver: Manual lexer + Generated parser
-- This isolates parser issues from lexer bugs
-- Uses JavaLexer (manual .x with start codes) + JavaParser (RTK-generated)
-- Note: JavaLexer.hs is generated from JavaLexer-manual.x, not from RTK
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
            putStrLn "Usage: java-main-hybrid <java-file>"
            exitFailure
