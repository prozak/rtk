{-# LANGUAGE QuasiQuotes #-}

-- End-to-end test of RTK quasi-quotation for the P grammar: pattern matching
-- with antiquote binders, construction with antiquote splices, and equality
-- against quasi-quoted expected values. Exits non-zero on any mismatch.
-- (AST equality ignores source positions: RtkPos compares equal by design.)

import Control.Monad (unless)
import System.Exit (exitFailure)

import PLexer
import PParser
import PQQ

-- Substitute value v for every free occurrence of variable x in an expression.
-- Quasi-quotation patterns destructure the AST; antiquoted construction
-- rebuilds it. Antiquote names must start with a rule shortcut (e for E,
-- id for Id, op1/op2 for the operator sorts).
subst :: Id -> E -> E -> E
subst x v expr = case expr of
  [e| $id1 |] -> if id1 == x then v else expr
  [e| (if0 $e1 $e2 $e3) |] ->
    let e1New = subst x v e1
        e2New = subst x v e2
        e3New = subst x v e3
    in [e| (if0 $e1New $e2New $e3New) |]
  [e| (fold $e1 $e2 (lambda ( $id1 $id2 ) $e3)) |] ->
    let e1New = subst x v e1
        e2New = subst x v e2
        -- the fold lambda binds id1 and id2; do not substitute under shadowing
        e3New = if x == id1 || x == id2 then e3 else subst x v e3
    in [e| (fold $e1New $e2New (lambda ( $id1 $id2 ) $e3New)) |]
  [e| ($op1a $e1) |] ->
    let e1New = subst x v e1
    in [e| ($op1a $e1New) |]
  [e| ($op2a $e1 $e2) |] ->
    let e1New = subst x v e1
        e2New = subst x v e2
    in [e| ($op2a $e1New $e2New) |]
  _ -> expr -- literals 0 and 1

main :: IO ()
main = do
  let prog = either errorWithoutStackTrace Prelude.id $
        scanTokens "(lambda (x) (fold x 0 (lambda (y z) (or y z))))" >>= parseP
  -- destructure the program with a quasi-quotation pattern
  [p| (lambda ($id1) $e1) |] <- return prog
  let result = subst id1 [e| 1 |] e1
      expected = [e| (fold 1 0 (lambda (y z) (or y z))) |]
  putStrLn $ "input:       " ++ show e1
  putStrLn $ "substituted: " ++ show result
  unless (result == expected) $ do
    putStrLn $ "EXPECTED:    " ++ show expected
    exitFailure
  -- shadowing: substituting y must not touch the fold lambda's body
  let shadowed = subst (Ctr__Id__0 rtkNoPos "y") [e| 1 |] e1
  unless (shadowed == e1) $ do
    putStrLn $ "shadowing broken: " ++ show shadowed
    exitFailure
  putStrLn "P quasi-quotation tests: PASS"
