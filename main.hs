import IO(hGetContents, bracket, openFile, IOMode(ReadMode), hClose)
import Lexer
import Parser
import Grammar

import Language.Haskell.TH

main = bracket (openFile "grammar.pg" ReadMode) (hClose)
    (\hndl ->
       do
          content <- hGetContents hndl
          let grammar = emitLoopsInGrammar . annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
          generateASTFile "AST" grammar
          generateQQFile "Quote" grammar
--          putStrLn $ show grammar)
          putStrLn $ (generateParserSpec grammar))

{--run :: String -> Either String [Token]
run content = runAlex content $ loop []

loop end = do --alexMonadScan >>= \t -> loop end >>= \e -> return $ t : e
  tok <- alexMonadScan;
  case tok of
    Nothing -> return end
    Just t -> loop end >>= \e -> return $ t : e--}

