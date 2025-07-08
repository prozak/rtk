import System.IO(readFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import Text.Show.Pretty

main = do
    args <- getArgs
    let file = case args of
                 f:_ -> f
                 _ -> "test-grammars/Simple.java"
    content <- readFile file
    putStrLn "=== Tokenizing ==="
    let tokens = alexScanTokens content
    putStrLn $ "Tokens: " ++ show (take 10 tokens)  -- show first 10 tokens
    
    putStrLn "\n=== Parsing ==="
    let ast = parseJava tokens
    putStrLn $ "AST: " ++ show ast
    
    putStrLn "\n=== Success! ==="