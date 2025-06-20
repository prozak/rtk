
{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import SandboxLexer
import SandboxParser
import SandboxQQ
import Text.Show.Pretty

getSandboxFileName = do
    args <- getArgs
    return $ case args of
                file:_ -> file
                _ -> error $ "Usage: <java-file>"

-- TODO: options parsing etc
main = do
    file <- getSandboxFileName
    content <- readFile file
    let tokens = alexScanTokens $ content
    
    -- Demonstrate parsing a simple Java class
    putStrLn "=== Tokens ==="
    putStrLn $ ppShow tokens
    
    return 0
