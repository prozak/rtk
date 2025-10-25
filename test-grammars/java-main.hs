import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
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

    putStrLn "\n=== Parse successful! ==="

    return 0
