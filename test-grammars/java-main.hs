{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import JavaQQ
import Text.Show.Pretty

getJavaFileName = do
    args <- getArgs
    return $ case args of
                file:_ -> file
                _ -> error $ "Usage: <java-file>"

-- TODO: options parsing etc
main = do
    file <- getJavaFileName
    content <- readFile file
    let ast = parseJava . alexScanTokens $ content
    
    -- Demonstrate parsing a simple Java class
    putStrLn "=== Parsed Java AST ==="
    putStrLn $ ppShow ast
    
    -- Demonstrate quasi-quoter functionality with Java expressions
    putStrLn "\n=== Java QuasiQuoter Demo ==="
    
    -- Example: create a simple Java expression using quasi-quotes
    let expr1 = [expression| x + y |]
    putStrLn $ "Simple expression: " ++ show expr1
    
    -- Example: create a Java method call
    let expr2 = [expression| obj.method(arg1, arg2) |]
    putStrLn $ "Method call: " ++ show expr2
    
    -- Example: pattern matching on Java constructs
    case ast of
        [java| class $CompoundName:className { $FieldDeclarationList:fields } |] -> do
            putStrLn $ "\nFound class: " ++ show className
            putStrLn $ "Fields: " ++ show fields
        _ -> putStrLn "\nNot a simple class declaration"
    
    return 0