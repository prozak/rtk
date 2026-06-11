{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import GrammarLexer
import GrammarParser
import GrammarQQ
import Text.Show.Pretty

getGrammarFileName = do
    args <- getArgs
    return $ case args of
                file:_ -> file
                _ -> error $ "Usage: <pg-file>"

-- The generated lexer and parser encode error positions as "LINE:COL:message"
-- (machine-splittable); render them back human-readably for the console
renderError :: String -> String
renderError err =
    case span (/= ':') err of
        (l, ':' : rest1) | [(line, "")] <- (reads l :: [(Int, String)]) ->
            case span (/= ':') rest1 of
                (c, ':' : msg) | [(col, "")] <- (reads c :: [(Int, String)]) ->
                    "line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
                _ -> err
        _ -> err

-- TODO: options parsing etc
main = do
    file <- getGrammarFileName
    content <- readFile file
    let grm = either (errorWithoutStackTrace . renderError) id $ scanTokens content >>= parseGrammar
    let [grammar|grammar $StrLit:str ; $importsOpt|] = [grammar|grammar 'test' ;|]
    let [rule|Rule = $cl1 | $clause2 | $clause3 | $clause4 ;|] = [rule| Rule = id '=' Clause ';' 
                                                                      | id ':' id '=' Clause ';'
                                                                      | id '.' id ':' id '=' Clause ';'
                                                                      | '.' id ':' id '=' Clause ';' ;|]
    --putStrLn $ show cl1
    --putStrLn $ show clause3
    --putStrLn $ show str
    let [grammar|grammar $StrLit:nm ; $importsOpt $ruleList|] = grm
    putStrLn $ show importsOpt
--    putStrLn $ ppShow [ruleList| $ruleList_rl1 RuleAAA = $Clause:cl1; |]
    return 0
