{
module PLexer(scanTokens, alexScanTokens, Token(..), PosToken(..), AlexPosn(..))
where

 }
%wrapper "monad"


tokens :- "tok_E_dummy_3" { simple Tk__tok_E_dummy_3 }
          "tok_Id_dummy_2" { simple Tk__tok_Id_dummy_2 }
          "tok_Op1_dummy_1" { simple Tk__tok_Op1_dummy_1 }
          "tok_Op2_dummy_0" { simple Tk__tok_Op2_dummy_0 }
          "tok_P_dummy_4" { simple Tk__tok_P_dummy_4 }
          "xor" { simple Tk__tok_xor_14 }
          "shr4" { simple Tk__tok_shr4_10 }
          "shr16" { simple Tk__tok_shr16_11 }
          "shr1" { simple Tk__tok_shr1_9 }
          "shl1" { simple Tk__tok_shl1_8 }
          "plus" { simple Tk__tok_plus_15 }
          "or" { simple Tk__tok_or_13 }
          "not" { simple Tk__tok_not_7 }
          "lambda" { simple Tk__tok_lambda_1 }
          "if0" { simple Tk__tok_if0_5 }
          "fold" { simple Tk__tok_fold_6 }
          "and" { simple Tk__tok_and_12 }
          "1" { simple Tk__tok_1_4 }
          "0" { simple Tk__tok_0_3 }
          ")" { simple Tk__tok__rparen__2 }
          "(" { simple Tk__tok__lparen__0 }
          ([\ \t\n]+) ;
          ([a-z]  [a-z_0-9]*) { simple1 $  Tk__id . (id) }
          ("$"  "Id"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Id . ((tail . dropWhile (/= ':'))) }
          ("$"  "Op2"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Op2 . ((tail . dropWhile (/= ':'))) }
          ("$"  "Op1"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Op1 . ((tail . dropWhile (/= ':'))) }
          ("$"  "E"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_E . ((tail . dropWhile (/= ':'))) }
          ("$"  "P"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_P . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_E_dummy_3 |
             Tk__tok_Id_dummy_2 |
             Tk__tok_Op1_dummy_1 |
             Tk__tok_Op2_dummy_0 |
             Tk__tok_P_dummy_4 |
             Tk__tok_xor_14 |
             Tk__tok_shr4_10 |
             Tk__tok_shr16_11 |
             Tk__tok_shr1_9 |
             Tk__tok_shl1_8 |
             Tk__tok_plus_15 |
             Tk__tok_or_13 |
             Tk__tok_not_7 |
             Tk__tok_lambda_1 |
             Tk__tok_if0_5 |
             Tk__tok_fold_6 |
             Tk__tok_and_12 |
             Tk__tok_1_4 |
             Tk__tok_0_3 |
             Tk__tok__rparen__2 |
             Tk__tok__lparen__0 |
             Tk__id String |
             Tk__qq_Id String |
             Tk__qq_Op2 String |
             Tk__qq_Op1 String |
             Tk__qq_E String |
             Tk__qq_P String
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