{
module DebugTestLexer(alexScanTokens, Token(..))
where

 }
%wrapper "monad"


tokens :- "tok_Assignment_dummy_18" { simple Tk__tok_Assignment_dummy_18 }
          "tok_Block_dummy_17" { simple Tk__tok_Block_dummy_17 }
          "tok_Expression_dummy_16" { simple Tk__tok_Expression_dummy_16 }
          "tok_Factor_dummy_15" { simple Tk__tok_Factor_dummy_15 }
          "tok_IfStatement_dummy_14" { simple Tk__tok_IfStatement_dummy_14 }
          "tok_Program_dummy_19" { simple Tk__tok_Program_dummy_19 }
          "tok_Statement_dummy_13" { simple Tk__tok_Statement_dummy_13 }
          "tok_Term_dummy_12" { simple Tk__tok_Term_dummy_12 }
          "tok_UnusedRule1_dummy_11" { simple Tk__tok_UnusedRule1_dummy_11 }
          "tok_UnusedRule2_dummy_10" { simple Tk__tok_UnusedRule2_dummy_10 }
          "tok_WhileLoop_dummy_9" { simple Tk__tok_WhileLoop_dummy_9 }
          "}" { simple Tk__tok__symbol__12 }
          "{" { simple Tk__tok__symbol__11 }
          "while" { simple Tk__tok_while_10 }
          "unused" { simple Tk__tok_unused_13 }
          "if" { simple Tk__tok_if_8 }
          "else" { simple Tk__tok_else_9 }
          "=" { simple Tk__tok__eql__0 }
          ";" { simple Tk__tok__semi__1 }
          "/" { simple Tk__tok__symbol__5 }
          "-" { simple Tk__tok__minus__3 }
          "+" { simple Tk__tok__plus__2 }
          "*" { simple Tk__tok__star__4 }
          ")" { simple Tk__tok__rparen__7 }
          "(" { simple Tk__tok__lparen__6 }
          (['0'-'9']+) { simple1 $  Tk__number . (id) }
          (['a'-'z'\ 'A'-'Z']  ['a'-'z'\ 'A'-'Z'\ '0'-'9']*) { simple1 $  Tk__identifier . (id) }
          ("$"  "UnusedRule2"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_UnusedRule2 . ((tail . dropWhile (/= ':'))) }
          ("$"  "UnusedRule1"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_UnusedRule1 . ((tail . dropWhile (/= ':'))) }
          ("$"  "Block"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Block . ((tail . dropWhile (/= ':'))) }
          ("$"  "WhileLoop"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_WhileLoop . ((tail . dropWhile (/= ':'))) }
          ("$"  "IfStatement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_IfStatement . ((tail . dropWhile (/= ':'))) }
          ("$"  "Factor"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Factor . ((tail . dropWhile (/= ':'))) }
          ("$"  "Term"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Term . ((tail . dropWhile (/= ':'))) }
          ("$"  "Expression"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Expression . ((tail . dropWhile (/= ':'))) }
          ("$"  "Assignment"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Assignment . ((tail . dropWhile (/= ':'))) }
          ("$"  "Statement"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Statement . ((tail . dropWhile (/= ':'))) }
          ("$"  "Program"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Program . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_Assignment_dummy_18 |
             Tk__tok_Block_dummy_17 |
             Tk__tok_Expression_dummy_16 |
             Tk__tok_Factor_dummy_15 |
             Tk__tok_IfStatement_dummy_14 |
             Tk__tok_Program_dummy_19 |
             Tk__tok_Statement_dummy_13 |
             Tk__tok_Term_dummy_12 |
             Tk__tok_UnusedRule1_dummy_11 |
             Tk__tok_UnusedRule2_dummy_10 |
             Tk__tok_WhileLoop_dummy_9 |
             Tk__tok__symbol__12 |
             Tk__tok__symbol__11 |
             Tk__tok_while_10 |
             Tk__tok_unused_13 |
             Tk__tok_if_8 |
             Tk__tok_else_9 |
             Tk__tok__eql__0 |
             Tk__tok__semi__1 |
             Tk__tok__symbol__5 |
             Tk__tok__minus__3 |
             Tk__tok__plus__2 |
             Tk__tok__star__4 |
             Tk__tok__rparen__7 |
             Tk__tok__lparen__6 |
             Tk__number String |
             Tk__identifier String |
             Tk__qq_UnusedRule2 String |
             Tk__qq_UnusedRule1 String |
             Tk__qq_Block String |
             Tk__qq_WhileLoop String |
             Tk__qq_IfStatement String |
             Tk__qq_Factor String |
             Tk__qq_Term String |
             Tk__qq_Expression String |
             Tk__qq_Assignment String |
             Tk__qq_Statement String |
             Tk__qq_Program String
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