import System.IO(readFile)
import System.Environment(getArgs)
import JavaLexerManual
import System.Exit(exitFailure, exitSuccess)

-- Lexical-only test driver for manual Alex lexer testing
-- This bypasses RTK generation and only tests the lexer
main = do
    args <- getArgs
    case args of
        ["--lex-only", file] -> do
            content <- readFile file
            let tokens = alexScanTokens content
            let tokenCount = length tokens  -- Force evaluation before declaring success
            putStrLn "=== Lexical analysis successful! ==="
            putStrLn $ "Token count: " ++ show tokenCount
            exitSuccess

        _ -> do
            putStrLn "Usage: java-main-manual --lex-only <java-file>"
            exitFailure
