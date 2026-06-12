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
    let [grammar|grammar $StrLit:str ; $ruleList|] = [grammar|grammar 'test' ;|]
    let [rule|Rule = $cl1 | $clause2 | $clause3 | $clause4 ;|] = [rule| Rule = id '=' Clause ';'
                                                                      | id ':' id '=' Clause ';'
                                                                      | id '.' id ':' id '=' Clause ';'
                                                                      | '.' id ':' id '=' Clause ';' ;|]
    -- A named alternative round-trips through a quote: the 'Mk:' label
    -- parses at quoter compile time and the pattern's metavariable binds the
    -- whole labeled clause at run time.
    let [rule|R = $clNamed ;|] = [rule|R = Mk: id '=' Clause ';' ;|]
    putStrLn $ show clNamed
    -- grammar.pg itself carries an imports block; project the grammar name
    -- out of the generated AST's named constructor
    let GrammarImports _ nm imports rules = grm
    putStrLn $ show nm
    return 0
