{
module SandboxLexer(alexScanTokens, Token(..))
where

 }
%wrapper "monad"


tokens :- "tok_Sandbox_dummy_0" { simple Tk__tok_Sandbox_dummy_0 }
          ("/*"  ([^\*]| [\*]  [^\/]| [\n])*  "*/") ;
          ("//"  .*) ;
          ([\ \t\n\r]+) ;
          ("/**"  ([^\*]| [\*]  [^\/]| [\n])*  "*/") { simple1 $  Tk__doccomment . (id) }
          ("$"  "Sandbox"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Sandbox . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_Sandbox_dummy_0 |
             Tk__doccomment String |
             Tk__qq_Sandbox String
             deriving (Show)

alexEOF = return EndOfFile
alexScanTokens :: String -> [Token]
alexScanTokens str = 
               case alexScanTokens1 str of
                  Right toks -> toks
                  Left err -> error err

alexScanTokens1 str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       EndOfFile -> return $ reverse toks
                       _ -> let toks' = tok : toks 
                            in toks' `seq` loop toks'
  loop []
simple1 :: (String -> Token) -> AlexInput -> Int -> Alex Token
simple1 t (_, _, _, str) len = return $ t (take len str)

simple t input len = return t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at " ++ (show line) ++ " line, " ++ (show column) ++ " column" ++ ". Following chars :" ++ (take 10 str)

}