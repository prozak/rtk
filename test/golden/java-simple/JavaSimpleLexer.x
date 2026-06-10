{
module JavaSimpleLexer(scanTokens, alexScanTokens, Token(..), PosToken(..), AlexPosn(..))
where

 }
%wrapper "monad"


tokens :- "tok_ClassDeclaration_dummy_10" { simple Tk__tok_ClassDeclaration_dummy_10 }
          "tok_CompilationUnit_dummy_9" { simple Tk__tok_CompilationUnit_dummy_9 }
          "tok_CompoundName_dummy_8" { simple Tk__tok_CompoundName_dummy_8 }
          "tok_Field_dummy_7" { simple Tk__tok_Field_dummy_7 }
          "tok_FieldList_dummy_6" { simple Tk__tok_FieldList_dummy_6 }
          "tok_JavaSimple_dummy_11" { simple Tk__tok_JavaSimple_dummy_11 }
          "tok_Package_dummy_5" { simple Tk__tok_Package_dummy_5 }
          "tok_Type_dummy_4" { simple Tk__tok_Type_dummy_4 }
          "}" { simple Tk__tok__symbol__5 }
          "{" { simple Tk__tok__symbol__4 }
          "public" { simple Tk__tok_public_2 }
          "package" { simple Tk__tok_package_0 }
          "int" { simple Tk__tok_int_6 }
          "class" { simple Tk__tok_class_3 }
          "String" { simple Tk__tok_String_7 }
          ";" { simple Tk__tok__semi__1 }
          "." { simple Tk__tok__dot__8 }
          ([\ \t\n\r]+) ;
          ([a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__id . (id) }
          ("$"  "CompoundName"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CompoundName . ((tail . dropWhile (/= ':'))) }
          ("$"  "Type"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Type . ((tail . dropWhile (/= ':'))) }
          ("$"  "Field"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Field . ((tail . dropWhile (/= ':'))) }
          ("$"  "FieldList"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_FieldList . ((tail . dropWhile (/= ':'))) }
          ("$"  "ClassDeclaration"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_ClassDeclaration . ((tail . dropWhile (/= ':'))) }
          ("$"  "Package"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_Package . ((tail . dropWhile (/= ':'))) }
          ("$"  "CompilationUnit"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_CompilationUnit . ((tail . dropWhile (/= ':'))) }
          ("$"  "JavaSimple"  ":"  [a-zA-Z_]  [A-Za-z0-9_]*) { simple1 $  Tk__qq_JavaSimple . ((tail . dropWhile (/= ':'))) }
          . { rtkError }

{
data Token = EndOfFile |
             Tk__tok_ClassDeclaration_dummy_10 |
             Tk__tok_CompilationUnit_dummy_9 |
             Tk__tok_CompoundName_dummy_8 |
             Tk__tok_Field_dummy_7 |
             Tk__tok_FieldList_dummy_6 |
             Tk__tok_JavaSimple_dummy_11 |
             Tk__tok_Package_dummy_5 |
             Tk__tok_Type_dummy_4 |
             Tk__tok__symbol__5 |
             Tk__tok__symbol__4 |
             Tk__tok_public_2 |
             Tk__tok_package_0 |
             Tk__tok_int_6 |
             Tk__tok_class_3 |
             Tk__tok_String_7 |
             Tk__tok__semi__1 |
             Tk__tok__dot__8 |
             Tk__id String |
             Tk__qq_CompoundName String |
             Tk__qq_Type String |
             Tk__qq_Field String |
             Tk__qq_FieldList String |
             Tk__qq_ClassDeclaration String |
             Tk__qq_Package String |
             Tk__qq_CompilationUnit String |
             Tk__qq_JavaSimple String
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