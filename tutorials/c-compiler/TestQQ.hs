{-# LANGUAGE QuasiQuotes #-}

-- Milestone-0 verification that the RTK quasi-quotation feature set works
-- end to end for the C grammar:
--
--   1. construction from C syntax
--   2. construction with scalar antiquote splices ($e)
--   3. construction with list antiquote splices ($stmts, also mixed with
--      literal syntax)
--   4. pattern matching with antiquote binders
--   5. pattern matching against literal syntax (non-matching case)
--   6. whole-list binding in patterns
--   7. quasi-quoted construction equals the file parser's output
module Main (main) where

import Prelude hiding (exp) -- the Exp quoter is named exp, like Prelude.exp

import Control.Monad (unless)
import System.Exit (exitFailure)

import CLexer (alexScanTokens)
import CParser
import CQQ

check :: String -> Bool -> IO Bool
check label ok = do
  putStrLn $ (if ok then "PASS  " else "FAIL  ") ++ label
  return ok

main :: IO ()
main = do
  results <-
    sequence
      [ check "construction: [exp| 42 |]" $
          [exp| 42 |] == Ctr__Exp__0 42
      , check "construction with scalar splice: [statement| return $e ; |]" $
          let e = [exp| 42 |]
          in [statement| return $e ; |] == Ctr__Statement__0 (Ctr__Exp__0 42)
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
              name == Ctr__Ident__0 "main" && length stmts == 2
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
  unless (and results) exitFailure
  putStrLn "All C quasi-quotation tests passed."

parse :: String -> Program
parse = parseC . alexScanTokens

expValue :: Exp -> Int
expValue (Ctr__Exp__0 n) = n
expValue other = error $ "expValue: unexpected expression node: " ++ show other
