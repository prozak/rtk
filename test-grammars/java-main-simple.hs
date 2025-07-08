{-# LANGUAGE OverloadedStrings #-}

import System.IO(readFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import Text.Show.Pretty

getJavaFileName = do
    args <- getArgs
    return $ case args of
                file:_ -> file
                _ -> "test-grammars/Complex.java"

main = do
    file <- getJavaFileName
    content <- readFile file

    putStrLn $ "=== Input Java Code " ++ file ++ "==="
    --putStrLn content

    --putStrLn "\n=== Tokenizing ==="
    let tokens = alexScanTokens content
    putStrLn $ "Number of tokens: " ++ show (length tokens)
    putStrLn $ "Tokens: " ++ show tokens

    --putStrLn "\n=== Parsing ==="
    --let ast = parseJava tokens
    --putStrLn $ "AST: " ++ ppShow ast

    putStrLn $ "=== Success! Java lexer works for " ++ file ++ " Number of tokens: " ++ show (length tokens) ++ "==="
    return 0
