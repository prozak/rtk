{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import Text.Show.Pretty

main = do
    args <- getArgs
    let file = case args of
                file:_ -> file
                _ -> "test-grammars/Test.java"

    content <- readFile file
    let tokens = alexScanTokens content
    let ast = parseJava tokens

    putStrLn "=== Tokenization successful ==="
    putStrLn $ "Token count: " ++ show (length tokens)

    putStrLn "\n=== Parsing successful ==="
    putStrLn $ ppShow ast

    putStrLn "\n=== Java grammar test PASSED ==="
    return ()
