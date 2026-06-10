{-# LANGUAGE QuasiQuotes #-}

-- Milestone-0 verification that the RTK quasi-quotation feature set works
-- end to end, for both generated grammars (C and assembly) living in one
-- program:
--
--   1. construction from concrete syntax
--   2. construction with scalar antiquote splices ($e, $src, $sym)
--   3. construction with list antiquote splices ($stmts, $items, also mixed
--      with literal syntax)
--   4. pattern matching with antiquote binders
--   5. pattern matching against literal syntax (non-matching case)
--   6. whole-list binding in patterns
--   7. quasi-quoted construction equals the file parser's output
--   8. assembly round trip: parsing emitted text yields the original AST
--   9. the full pipeline: parse C -> codegen -> emit -> parse Asm
--
-- AST equality is position-transparent (RtkPos compares equal by design),
-- which is what makes 7-9 possible: compile-time-parsed quotes match
-- runtime-parsed files.
module Main (main) where

import Prelude hiding (exp) -- the Exp quoter is named exp, like Prelude.exp

import Control.Monad (unless)
import System.Exit (exitFailure)

import CLexer (scanTokens)
import CParser
import CQQ

import qualified AsmLexer
import AsmParser hiding (rtkNoPos) -- both parsers export rtkNoPos (distinct types)
import qualified AsmParser as A
import AsmQQ

import Codegen (codegen)
import Emit (emit)

check :: String -> Bool -> IO Bool
check label ok = do
  putStrLn $ (if ok then "PASS  " else "FAIL  ") ++ label
  return ok

main :: IO ()
main = do
  putStrLn "-- C grammar --"
  cResults <-
    sequence
      [ check "construction: [exp| 42 |]" $
          [exp| 42 |] == Ctr__Exp__0 rtkNoPos 42
      , check "construction with scalar splice: [statement| return $e ; |]" $
          let e = [exp| 42 |]
          in [statement| return $e ; |]
               == Ctr__Statement__0 rtkNoPos (Ctr__Exp__0 rtkNoPos 42)
      , check "pattern with antiquote binder: [statement| return $e1 ; |]" $
          case [statement| return 7 ; |] of
            [statement| return $e1 ; |] -> e1 == [exp| 7 |]
      , check "pattern against literal syntax (incl. non-match)" $
          let classify s = case s of
                [statement| return 8 ; |] -> "eight"
                [statement| return $e1 ; |] -> "other " ++ show (expValue e1)
                _ -> "no match"
          in classify [statement| return 8 ; |] == "eight"
               && classify [statement| return 7 ; |] == "other 7"
      , check "whole-list pattern binding: { $stmts }" $
          case parse "int main() { return 1; return 2; }" of
            [program| int $name ( ) { $stmts } |] ->
              name == Ctr__Ident__0 rtkNoPos "main" && length stmts == 2
            _ -> False
      , check "list splice in construction: { $stmts0 }" $
          let stmts0 = [[statement| return 1 ; |], [statement| return 2 ; |]]
          in [program| int main ( ) { $stmts0 } |]
               == parse "int main() { return 1; return 2; }"
      , check "mixed list splice: { $stmts0 return 9 ; }" $
          let stmts0 = [[statement| return 1 ; |]]
          in [program| int main ( ) { $stmts0 return 9 ; } |]
               == parse "int main() { return 1; return 9; }"
      , check "quasi-quoted construction == parsed file" $
          [program| int main ( ) { return 42 ; } |]
            == parse "int main() {\n  // a comment\n  return 42; /* another */\n}\n"
      ]

  putStrLn "-- assembly grammar --"
  asmResults <-
    sequence
      [ check "construction: [operand| $5 |] and [operand| %eax |]" $
          [operand| $5 |] == Ctr__Operand__0 A.rtkNoPos 5
            && [operand| %eax |]
                 == Ctr__Operand__1 A.rtkNoPos (Ctr__Reg__0 A.rtkNoPos)
      , check "construction with scalar splice: movl $src, %eax" $
          let src = Ctr__Operand__0 A.rtkNoPos 7
          in [asmItem| movl $src, %eax |]
               == Ctr__AsmItem__2 A.rtkNoPos
                    (Ctr__Operand__0 A.rtkNoPos 7)
                    (Ctr__Operand__1 A.rtkNoPos (Ctr__Reg__0 A.rtkNoPos))
      , check "pattern with antiquote binders: movl $src, $dst" $
          case [asmItem| movl $3, %eax |] of
            [asmItem| movl $src, $dst |] ->
              src == [operand| $3 |] && dst == [operand| %eax |]
            _ -> False
      , check "scalar + list splices: .globl $sym / $sym : / $items" $
          let sym = Ctr__AsmId__0 A.rtkNoPos "main"
              items = [asmItems| movl $2, %eax
                                 ret |]
          in [asm| .globl $sym
                   $sym :
                   $items |]
               == parseAsmText "    .globl main\nmain:\n    movl    $2, %eax\n    ret\n"
      , check "round trip: parse (emit asm) == asm" $
          let prog = [asm| .globl main
                           main :
                           movl $42, %eax
                           ret |]
          in parseAsmText (emit prog) == prog
      , check "full pipeline: parse C -> codegen -> emit -> parse Asm" $
          parseAsmText (emit (codegen (parse "int main() { return 2; }")))
            == [asm| .globl main
                     main :
                     movl $2, %eax
                     ret
                     movl $0, %eax
                     ret |]
      ]

  unless (and (cResults ++ asmResults)) exitFailure
  putStrLn "All quasi-quotation tests passed."

parse :: String -> Program
parse src = either error id (scanTokens src >>= parseC)

parseAsmText :: String -> Asm
parseAsmText src = either error id (AsmLexer.scanTokens src >>= parseAsm)

expValue :: Exp -> Int
expValue (Ctr__Exp__0 _ n) = n
expValue other = error $ "expValue: unexpected expression node: " ++ show other
