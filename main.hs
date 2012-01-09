import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import Lexer
import Parser
import Grammar
import Text.Show.Pretty
import StringLiterals
import Normalize
import PrintGrammar
import GenY
import GenX

import Language.Haskell.TH

getGrammarFileName = do
    args <- getArgs
    return $ case args of
                file:dir:_ -> (file, dir)
                _ -> error $ "Usage: <pg-file> <output-directory>"

-- TODO: options parsing etc
main = do
    (file, dir) <- getGrammarFileName
    content <- readFile file
    let grammar = parse . alexScanTokens $ content
--    let grammar = emitLoopsInGrammar . annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
--    let grammar = annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
--    generateASTFile "AST" grammar
--    generateQQFile "Quote" grammar
    let grammar0 = normalizeStringLiterals $ addDefaults grammar
--    putStrLn "------ before noralization ------"
--    putStrLn $ showGrammar grammar0
    let grammar1 = fillConstructorNames $ normalizeTopLevelClauses grammar0
--    putStrLn "------ after noralization ------"
--    putStrLn $ showGrammar grammar1
    let grammar_name = getGrammarName grammar1
    let y_content = genY grammar1
    let x_content = genX grammar1
    writeFile (dir ++ "/" ++ grammar_name ++ "Parser.y") y_content
    writeFile (dir ++ "/" ++ grammar_name ++ "Lexer.x") x_content
--    putStrLn $ ppShow grammar1)
--    putStrLn $ (generateParserSpec grammar))

{--run :: String -> Either String [Token]
run content = runAlex content $ loop []

loop end = do --alexMonadScan >>= \t -> loop end >>= \e -> return $ t : e
  tok <- alexMonadScan;
  case tok of
    Nothing -> return end
    Just t -> loop end >>= \e -> return $ t : e--}

