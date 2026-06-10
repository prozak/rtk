{
module T1Lexer(scanTokens, alexScanTokens, Token(..), PosToken(..), AlexPosn(..))
where

 }
%wrapper "monad"


tokens :- "tok_A_dummy_19" { simple Tk__tok_A_dummy_19 }
          "tok_B_dummy_18" { simple Tk__tok_B_dummy_18 }
          "tok_C_dummy_17" { simple Tk__tok_C_dummy_17 }
          "tok_D_dummy_16" { simple Tk__tok_D_dummy_16 }
          "tok_E_dummy_15" { simple Tk__tok_E_dummy_15 }
          "tok_F1_dummy_14" { simple Tk__tok_F1_dummy_14 }
          "tok_F2_dummy_13" { simple Tk__tok_F2_dummy_13 }
          "tok_F3_dummy_12" { simple Tk__tok_F3_dummy_12 }
          "tok_F4_dummy_11" { simple Tk__tok_F4_dummy_11 }
          "tok_F5_dummy_10" { simple Tk__tok_F5_dummy_10 }
          "tok_G_dummy_9" { simple Tk__tok_G_dummy_9 }
          "b" { simple Tk__tok_b_1 }
          "a" { simple Tk__tok_a_0 }
          "," { simple Tk__tok__coma__2 }
          ("$"  "G"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_G . ((tail . dropWhile (/= ':'))) }
          ("$"  "F5"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_F5 . ((tail . dropWhile (/= ':'))) }
          ("$"  "F4"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_F4 . ((tail . dropWhile (/= ':'))) }
          ("$"  "F3"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_F3 . ((tail . dropWhile (/= ':'))) }
          ("$"  "F2"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_F2 . ((tail . dropWhile (/= ':'))) }
          ("$"  "F1"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_F1 . ((tail . dropWhile (/= ':'))) }
          ("$"  "E"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_E . ((tail . dropWhile (/= ':'))) }
          ("$"  "D"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_D . ((tail . dropWhile (/= ':'))) }
          ("$"  "C"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_C . ((tail . dropWhile (/= ':'))) }
          ("$"  "B"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_B . ((tail . dropWhile (/= ':'))) }
          ("$"  "A"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_A . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_A_dummy_19 |
             Tk__tok_B_dummy_18 |
             Tk__tok_C_dummy_17 |
             Tk__tok_D_dummy_16 |
             Tk__tok_E_dummy_15 |
             Tk__tok_F1_dummy_14 |
             Tk__tok_F2_dummy_13 |
             Tk__tok_F3_dummy_12 |
             Tk__tok_F4_dummy_11 |
             Tk__tok_F5_dummy_10 |
             Tk__tok_G_dummy_9 |
             Tk__tok_b_1 |
             Tk__tok_a_0 |
             Tk__tok__coma__2 |
             Tk__qq_G String |
             Tk__qq_F5 String |
             Tk__qq_F4 String |
             Tk__qq_F3 String |
             Tk__qq_F2 String |
             Tk__qq_F1 String |
             Tk__qq_E String |
             Tk__qq_D String |
             Tk__qq_C String |
             Tk__qq_B String |
             Tk__qq_A String
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