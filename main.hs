import System.IO(readFile)
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
                s:_ -> s
                [] -> error $ "Usage: <file>"

-- TODO: options parsing etc
main = do
    file <- getGrammarFileName
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
    putStrLn $ genY grammar1
    putStrLn $ genX grammar1
--    putStrLn $ ppShow grammar1)
--    putStrLn $ (generateParserSpec grammar))

{--run :: String -> Either String [Token]
run content = runAlex content $ loop []

loop end = do --alexMonadScan >>= \t -> loop end >>= \e -> return $ t : e
  tok <- alexMonadScan;
  case tok of
    Nothing -> return end
    Just t -> loop end >>= \e -> return $ t : e--}

