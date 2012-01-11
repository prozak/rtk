import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import GrammarLexer
import GrammarParser
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
    let grammar = parseGrammar . alexScanTokens $ content
    putStrLn $ ppShow grammar
