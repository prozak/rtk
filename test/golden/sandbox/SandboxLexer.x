{
module SandboxLexer(scanTokens, alexScanTokens, Token(..), PosToken(..), AlexPosn(..))
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

-- A token together with the source position where it starts
data PosToken = PosToken { ptPos :: AlexPosn, ptToken :: Token }
                deriving (Show)

alexEOF = do
  (pos, _, _, _) <- alexGetInput
  return $ PosToken pos EndOfFile

-- Lex the input into a token stream, returning the positioned error message
-- on a lexical error. The returned list always ends with an EndOfFile token
-- that carries the position of the end of input, so parse errors at end of
-- input can be reported with a position too
scanTokens :: String -> Either String [PosToken]
scanTokens str = runAlex str $ do
  let loop toks = do tok <- alexMonadScan
                     case tok of
                       PosToken _ EndOfFile -> return $ reverse (tok : toks)
                       _ -> let toks' = tok : toks
                            in toks' `seq` loop toks'
  loop []

-- Thin compatibility wrapper: callers that have not switched to 'scanTokens'
-- get the error message thrown instead
alexScanTokens :: String -> [PosToken]
alexScanTokens str =
               case scanTokens str of
                  Right toks -> toks
                  Left err -> errorWithoutStackTrace err

simple1 :: (String -> Token) -> AlexInput -> Int -> Alex PosToken
simple1 t (pos, _, _, str) len = return $ PosToken pos (t (take len str))

simple :: Token -> AlexInput -> Int -> Alex PosToken
simple t (pos, _, _, _) len = return $ PosToken pos t

rtkError ((AlexPn _ line column), _, _, str) len = alexError $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column) ++ ". Following chars: " ++ (take 10 str)

}