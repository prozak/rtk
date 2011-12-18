import IO(hGetContents, bracket, openFile, IOMode(ReadMode), hClose)
import Lexer
import Parser

main = bracket (openFile "grammar.pg" ReadMode) (hClose)
    (\hndl ->
       do
          content <- hGetContents hndl
          let res = parse $ alexScanTokens content
          print $ res)

{--run :: String -> Either String [Token]
run content = runAlex content $ loop []

loop end = do --alexMonadScan >>= \t -> loop end >>= \e -> return $ t : e
  tok <- alexMonadScan;
  case tok of
    Nothing -> return end
    Just t -> loop end >>= \e -> return $ t : e--}

