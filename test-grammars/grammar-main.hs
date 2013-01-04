{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import GrammarLexer
import GrammarParser
import GrammarQQ
import Text.Show.Pretty
import qualified Language.Haskell.TH as TH

getGrammarFileName = do
    args <- getArgs
    return $ case args of
                file:_ -> file
                _ -> error $ "Usage: <pg-file>"

-- TODO: options parsing etc
main = do
    file <- getGrammarFileName
    content <- readFile file
    let grm = parseGrammar . alexScanTokens $ content
    let [grammar|grammar $StrLit:str ;|] = [grammar|grammar 'test' ;|]
    let [rule|Rule = $cl1 | $clause2 | $clause3 | $clause4 ;|] = [rule| Rule = id '=' Clause ';' 
                                                                      | id ':' id '=' Clause ';'
                                                                      | id '.' id ':' id '=' Clause ';'
                                                                      | '.' id ':' id '=' Clause ';' ;|]
    putStrLn $ show cl1
    putStrLn $ show clause3
    putStrLn $ show str
    let [grammar|grammar $StrLit:nm; @shortcuts(gr, $idList1) $r1 $ruleList_rl1|] = [grammar|
                            grammar 'Grammar';
                                    
                            @shortcuts(gr, gra, grrr)
                            Grammar = 'grammar' str ';' Rule* ;
                                      
                            @shortcuts(r, ru)
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
    --putStrLn $ show rl
    putStrLn $ ppShow [ruleList| $ruleList_rl1 RuleAAA = $Clause:cl1; |]
    return 0
