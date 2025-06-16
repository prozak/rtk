{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile)
import System.Environment(getArgs)
import JavaSimpleLexer
import JavaSimpleParser
import JavaSimpleQQ
import Text.Show.Pretty

main = do
    args <- getArgs
    let file = case args of
                 f:_ -> f
                 _ -> "test-grammars/Simple.java"
    content <- readFile file
    putStrLn "=== Input Java Code ==="
    putStrLn content
    
    putStrLn "\n=== Tokenizing ==="
    let tokens = alexScanTokens content
    putStrLn $ "First 10 tokens: " ++ show (take 10 tokens)
    
    putStrLn "\n=== Parsing ==="
    let ast = parseJavaSimple tokens
    putStrLn $ "AST: " ++ ppShow ast
    
    putStrLn "\n=== Java QuasiQuoter Demo ==="
    -- QuasiQuoter functionality available (but simplified for this demo)
    putStrLn "QuasiQuoters are available for: java, classDeclaration, type, field, etc."
    
    putStrLn "\n=== Success! Java parsing works! ====="