import System.IO(hGetContents, openFile, IOMode(ReadMode), hClose)
import Control.Exception(bracket)
import Lexer
import Parser
import Grammar
import Text.Show.Pretty
import StringLiterals
import Normalize

import Language.Haskell.TH

main = bracket (openFile "grammar.pg" ReadMode) (hClose)
    (\hndl ->
       do
          content <- hGetContents hndl
          let grammar = parse . alexScanTokens $ content
--          let grammar = emitLoopsInGrammar . annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
--          let grammar = annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
--          generateASTFile "AST" grammar
--          generateQQFile "Quote" grammar
          let grammar1 = normalizeTopLevelClauses $ normalizeStringLiterals $ addDefaults grammar
          putStrLn $ ppShow grammar1)
--          putStrLn $ (generateParserSpec grammar))

{--run :: String -> Either String [Token]
run content = runAlex content $ loop []

loop end = do --alexMonadScan >>= \t -> loop end >>= \e -> return $ t : e
  tok <- alexMonadScan;
  case tok of
    Nothing -> return end
    Just t -> loop end >>= \e -> return $ t : e--}

