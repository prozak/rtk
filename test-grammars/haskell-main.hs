{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import HaskellLexer
import HaskellParser
import HaskellQQ
import Text.Show.Pretty

getGrammarFileName = do
    args <- getArgs
    return $ case args of
                file:_ -> file
                _ -> error $ "Usage: <pg-file>"

-- TODO: options parsing etc
main = do
    file <- getGrammarFileName
    content <- readFile file
    let lexems = alexScanTokens content
    let prg = parseHaskell . alexScanTokens $ content
    putStrLn $ show lexems
    --putStrLn $ show prg
    return 0
