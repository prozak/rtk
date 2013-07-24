module Main 
    where
      
import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import Lexer
import Parser
import Text.Show.Pretty

import Control.Monad.Identity

import Language.Haskell.TH
import GenClasses
import GenXY
import GenAST
import Generator
import MonadFuture


getGrammarFileName = do
    args <- getArgs
    return $ case args of
                file:dir:_ -> (file, dir)
                _ -> error $ "Usage: <pg-file> <output-directory>"

myGenerate :: XYGen (SimpleASTGen Identity) b -> XYGen (SimpleASTGen Identity) a -> Either [String] a
myGenerate recComp normComp = let ((r, parserState), astState) = runIdentity $ runASTGenRec $ runParserGenRec (recComp >> getErrors)
                              in case r of
                                   [] -> Right $ runIdentity $ runASTGen astState $ runParserGen parserState normComp
                                   _ -> Left r

-- TODO: options parsing etc
main = do
    (file, dir) <- getGrammarFileName
    content <- readFile file
    let grammar = parse . alexScanTokens $ content
    print grammar
    let res = myGenerate (generateGrammar grammar) (generateContent (getIGrammarName grammar))
    --let res = runIdentity $ runASTGen $ generateTest
    case res of
      Right res -> mapM_ (\ (fname, cont) -> writeFile (dir ++ "/" ++ fname) cont) res
      Left errors -> do
               putStrLn "Errors:"
               mapM_ putStrLn errors
--    let grammar = emitLoopsInGrammar . annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
--    let grammar = annotateGrammarWithNames . addStartRule . parse . alexScanTokens $ content
--    generateASTFile "AST" grammar
--    generateQQFile "Quote" grammar
---    let grammar0 = normalizeStringLiterals grammar
--    putStrLn "------ before noralization ------"
--    putStrLn $ showGrammar grammar0
---    let grammar1 = fillConstructorNames $ normalizeTopLevelClauses grammar0
--    putStrLn "------ after noralization ------"
--    putStrLn $ showGrammar grammar1
---    let grammar_name = getNGrammarName grammar1
---    let y_content = genY grammar1
---    let x_content = genX grammar1
---    let q_content = genQ grammar1
---    let info = getGrammarInfo grammar1
--    putStrLn $ ppShow grammar1
--    putStrLn $ show $ getStartRuleName info
--    putStrLn $ show $ getProxyRules info
---    writeFile (dir ++ "/" ++ grammar_name ++ "Parser.y") y_content
--    putStrLn $ show $ getRuleToStartInfo info
---    writeFile (dir ++ "/" ++ grammar_name ++ "Lexer.x") x_content
--    putStrLn $ show $ getRuleToAntiInfo info
---    writeFile (dir ++ "/" ++ grammar_name ++ "QQ.hs") q_content
--    putStrLn $ (generateParserSpec grammar))
    return ()

{--run :: String -> Either String [Token]
run content = runAlex content $ loop []

loop end = do --alexMonadScan >>= \t -> loop end >>= \e -> return $ t : e
  tok <- alexMonadScan;
  case tok of
    Nothing -> return end
    Just t -> loop end >>= \e -> return $ t : e--}

