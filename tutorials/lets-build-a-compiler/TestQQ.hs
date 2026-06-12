{-# LANGUAGE QuasiQuotes #-}

-- Milestone-0 test suite: the full RTK quasi-quotation feature checklist,
-- exercised against calc.pg. The evaluator doubles as Crenshaw's part 4
-- (an interpreter) in miniature: every language construct is destructured
-- with a quasi-quotation pattern, never with a raw constructor match.
--
-- Token payloads (the integer literal, the variable name) cannot be bound
-- by an antiquote ($x works on whole syntax sorts only), so leaf nodes go
-- through one hand-written destructor (litVal). AST equality ignores
-- source positions (RtkPos compares equal by design), which is what lets
-- parsed, constructed and spliced trees be compared directly.
--
-- Exits non-zero on any mismatch. Run with: make test

import qualified Data.Map as M
import System.Exit (exitFailure)

import CalcLexer
import CalcParser
import CalcQQ

parseOrDie :: String -> Prog
parseOrDie src = either errorWithoutStackTrace (\v -> v) (scanTokens src >>= parseCalc)

-- ---------------------------------------------------------------------------
-- The interpreter (Crenshaw part 4, on an AST instead of during the parse)
-- ---------------------------------------------------------------------------

type Env = M.Map Var Int

-- leaf destructor: the one place a generated constructor is unavoidable
litVal :: Lit -> Int
litVal (Ctr__Lit__0 _ n) = n
litVal other = error $ "litVal: unexpected literal node: " ++ show other

evalE :: Env -> Expr -> Int
evalE env [expr| $e1 + $e2 |] = evalE env e1 + evalE env e2
evalE env [expr| $e1 - $e2 |] = evalE env e1 - evalE env e2
evalE env [expr| $e1 * $e2 |] = evalE env e1 * evalE env e2
evalE env [expr| $e1 / $e2 |] = evalE env e1 `div` evalE env e2
evalE env [expr| - $e1 |]     = negate (evalE env e1)
evalE _   [expr| $lit1 |]     = litVal lit1
evalE env [expr| $var1 |]     =
    case M.lookup var1 env of
      Just v  -> v
      Nothing -> error $ "evalE: unbound variable: " ++ show var1
evalE _ other = error $ "evalE: unmatched expression: " ++ show other

runStmt :: (Env, [Int]) -> Stmt -> (Env, [Int])
runStmt (env, out) [stmt| $var1 = $e1 |]     = (M.insert var1 (evalE env e1) env, out)
runStmt (env, out) [stmt| print($e1) |]      = (env, out ++ [evalE env e1])
runStmt (env, out) [stmt| print($e1, $e2) |] = (env, out ++ [evalE env e1, evalE env e2])
runStmt _ other = error $ "runStmt: unmatched statement: " ++ show other

runProg :: Prog -> [Int]
runProg [prog| $bodyAll |] = snd $ foldl runStmt (M.empty, []) bodyAll
runProg other = error $ "runProg: unmatched program: " ++ show other

-- ---------------------------------------------------------------------------
-- Test driver
-- ---------------------------------------------------------------------------

check :: (Eq a, Show a) => String -> a -> a -> IO Bool
check name got want
  | got == want = do putStrLn $ "PASS: " ++ name
                     return True
  | otherwise   = do putStrLn $ "FAIL: " ++ name
                     putStrLn $ "  expected: " ++ show want
                     putStrLn $ "  got:      " ++ show got
                     return False

main :: IO ()
main = do
    let prg = parseOrDie
          "x = 2 + 3 * 4;          { precedence }\n\
          \y = (x - 2) * 2;        { grouping }\n\
          \print(x, y);\n\
          \print(-y / 2 + 1)       { unary minus }"

    -- whole-body list antiquote in a let pattern
    let [prog| $bodyXs |] = prg

    -- list splice in construction: reuse the parsed statements, append two
    let prg2 = [prog| $bodyXs ; z = x + 1 ; print(z) |]

    -- construction with nested antiquote splices
    let e_five = [expr| 5 |]
        e_big  = [expr| $e_five * ($e_five + 1) |]

    results <- sequence
      [ check "parse + eval (typed Int tokens, sep lists, precedence, comments)"
              (runProg prg) [14, 24, -11]
      , check "whole-list pattern binds all statements"
              (length bodyXs) 4
      , check "stmt pattern with Var/Expr antiquotes"
              (case head bodyXs of
                 [stmt| $var1 = $e1 |] -> (var1 == [var| x |], evalE M.empty e1)
                 _                     -> (False, 0))
              (True, 14)
      , check "precedence shapes patterns: 1 + 2 * 3 splits at '+'"
              (case [expr| 1 + 2 * 3 |] of
                 [expr| $e1 + $e2 |] -> (evalE M.empty e1, evalE M.empty e2)
                 _                   -> (-1, -1))
              (1, 6)
      , check "parens are lifted away: (1) + 2 == 1 + 2"
              [expr| (1) + 2 |] [expr| 1 + 2 |]
      , check "antiquote splice into construction" (evalE M.empty e_big) 30
      , check "list splice + construction runs"
              (runProg prg2) [14, 24, -11, 15]
      ]

    if and results
      then putStrLn $ "All " ++ show (length results) ++ " Calc QQ tests passed."
      else exitFailure
