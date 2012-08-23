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

-- TODO: options parsing etc
main = do
    file <- getGrammarFileName
    content <- readFile file
--    let grammar = parseGrammar . alexScanTokens $ content
    let rl = [rule| Rule = id '=' Clause ';' 
                                 | id ':' id '=' Clause ';'
                                 | id '.' id ':' id '=' Clause ';'
                                 | '.' id ':' id '=' Clause ';' ;|]
    let grm = [grammar|
                            grammar 'Grammar';

                            Grammar = 'grammar' str ';' Rule* ;

                            Rule = id '=' Clause ';' 
                                 | id ':' id '=' Clause ';'
                                 | id '.' id ':' id '=' Clause ';'
                                 | '.' id ':' id '=' Clause ';' ;

                            Clause = Clause2 + ~'|' ;

                            Clause: Clause2 = Clause3 * ;

                            Clause: Clause3 = ',' Clause4 
                                            | '!' Clause4
                                            | ,Clause4 ;

                            Clause: Clause4 = Clause5 '*' OptDelim
                                            | Clause5 '+' OptDelim
                                            | Clause5 '?'
                                            | ,Clause5 ;

                            Clause: Clause5 = '(' ,Clause ')'
                                            | id
                                            | str
                                            | '.'
                                            | regexplit ;

                            OptDelim = ('~' Clause5)? ;

                            id = [a-zA-Z][A-Za-z0-9_]* ;
                            str = '\'' ([^'] | '\\\'')* '\'' ;
                            regexplit = '[' ([^\]] | '\\]')* ']' ;
                            Ignore: ws = [ \t\n]+ ;
                            Ignore: comment = '#' .* '\n' ;
    |]
    putStrLn $ show rl
--    putStrLn $ ppShow grammar
