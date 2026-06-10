{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- Phase-0 spike for "Write You a Haskell" on RTK.
--
-- Exercises every quasi-quotation mode on the lc.pg grammar (operator
-- ladder + juxtaposition application, the shape of the tutorial's Poly
-- language): construction, pattern matching, splicing, binder metavars,
-- and a miniature call-by-value evaluator written only with QQ patterns.

import Data.Generics (everywhere, mkT)
import Data.IORef
import System.Exit (exitFailure)

import LcLexer
import LcParser
import LcQQ

-- The only raw generated constructors the spike touches: integer literals
-- (Int cannot be antiquoted) and identifier construction. Phase 1 promotes
-- these to a full pattern-synonym module. The synonyms are explicitly
-- bidirectional: matching ignores the constructors' RtkPos field,
-- construction supplies rtkNoPos.
pattern LitI :: Int -> Expr
pattern LitI n <- Ctr__Expr__0 _ n
  where LitI n = Ctr__Expr__0 rtkNoPos n

pattern IdN :: String -> Id
pattern IdN s <- Ctr__Id__0 _ s
  where IdN s = Ctr__Id__0 rtkNoPos s

-- Unwrapping the start symbol is itself a QQ pattern on the start rule.
unLc :: Lc -> Expr
unLc [lc| $e |] = e
unLc other      = error $ "unexpected start wrapper: " ++ show other

-- Prelude.id: LcQQ also exports an 'id' quasi-quoter for the Id type
parseExpr :: String -> Expr
parseExpr src =
    unLc $ either errorWithoutStackTrace Prelude.id (scanTokens src >>= parseLc)

-- --------------------------------------------------------------------
-- A chapter-4-style interpreter, written against concrete syntax.
-- --------------------------------------------------------------------

-- Substitution via SYB: Var occurrences are matched with a QQ pattern,
-- binder Ids are untouched because the traversal only rewrites Exprs.
-- (Capture-naive, which is fine for the spike's closed test terms.)
subst :: Id -> Expr -> Expr -> Expr
subst x v = everywhere (mkT step)
  where step e@[expr| $x1 |] | x1 == x = v
        step e                         = e

eval :: Expr -> Expr
eval [expr| $e1 + $e2 |]  = arith (+) e1 e2
eval [expr| $e1 - $e2 |]  = arith (-) e1 e2
eval [expr| $e1 * $e2 |]  = arith (*) e1 e2
eval [expr| $e1 == $e2 |] =
    if eval e1 == eval e2 then [expr| true |] else [expr| false |]
eval [expr| if $e1 then $e2 else $e3 |] =
    if eval e1 == [expr| true |] then eval e2 else eval e3
eval [expr| let $x1 = $e1 in $e2 |] = eval (subst x1 (eval e1) e2)
eval [expr| $e1 $e2 |] = case eval e1 of
    [expr| fn $x1 -> $e3 |] -> eval (subst x1 (eval e2) e3)
    other                   -> error $ "stuck application on: " ++ show other
eval e = e  -- literals, booleans, lambdas and free variables are values

arith :: (Int -> Int -> Int) -> Expr -> Expr -> Expr
arith op e1 e2 = case (eval e1, eval e2) of
    (LitI a, LitI b) -> LitI (a `op` b)
    (v1, v2)         -> error $ "stuck arithmetic on: " ++ show (v1, v2)

-- Pattern-quote classification across all ladder levels; the binary
-- operator cases are exactly what fails for the Java grammar.
describe :: Expr -> String
describe [expr| fn $x1 -> $e1 |]            = "lam"
describe [expr| let $x1 = $e1 in $e2 |]     = "let"
describe [expr| if $e1 then $e2 else $e3 |] = "if"
describe [expr| $e1 == $e2 |]               = "eq"
describe [expr| $e1 + $e2 |]                = "add"
describe [expr| $e1 - $e2 |]                = "sub"
describe [expr| $e1 * $e2 |]                = "mul"
describe [expr| $e1 $e2 |]                  = "app"
describe [expr| $x1 |]                      = "var"
describe _                                  = "lit"

-- --------------------------------------------------------------------
-- Test harness
-- --------------------------------------------------------------------

main :: IO ()
main = do
    failures <- newIORef (0 :: Int)
    let check :: (Eq a, Show a) => String -> a -> a -> IO ()
        check label actual expected
            | actual == expected = putStrLn $ "PASS  " ++ label
            | otherwise = do
                putStrLn $ "FAIL  " ++ label
                putStrLn $ "      expected: " ++ show expected
                putStrLn $ "      actual:   " ++ show actual
                modifyIORef failures (+ 1)

    putStrLn "== construction: precedence and associativity =="
    check "* binds tighter than +"
        [expr| 1 + 2 * 3 |] [expr| 1 + (2 * 3) |]
    check "- is left associative"
        [expr| 1 - 2 - 3 |] [expr| (1 - 2) - 3 |]
    check "application is left associative"
        [expr| f x y |] [expr| (f x) y |]
    check "application binds tighter than operators"
        [expr| f x + g y == 5 |] [expr| ((f x) + (g y)) == 5 |]
    check "+ is not commutative on ASTs"
        ([expr| 1 + 2 |] == [expr| 2 + 1 |]) False

    putStrLn "== construction: quotes agree with runtime parses =="
    check "lambda quote = parsed lambda"
        [expr| fn x -> x + 1 |] (parseExpr "fn x -> x + 1")
    check "let/if quote = parsed let/if"
        [expr| let y = 1 in if y == 1 then true else false |]
        (parseExpr "let y = 1 in if y == 1 then true else false")

    putStrLn "== pattern quotes at every precedence level =="
    check "describe lam" (describe (parseExpr "fn x -> x")) "lam"
    check "describe let" (describe (parseExpr "let x = 1 in x")) "let"
    check "describe if"  (describe (parseExpr "if true then 1 else 2")) "if"
    check "describe eq"  (describe (parseExpr "f x == 1")) "eq"
    check "describe add" (describe (parseExpr "1 + f x")) "add"
    check "describe sub" (describe (parseExpr "1 - 2")) "sub"
    check "describe mul" (describe (parseExpr "2 * 2")) "mul"
    check "describe app" (describe (parseExpr "f x")) "app"
    check "describe var" (describe (parseExpr "free")) "var"
    check "describe lit" (describe (parseExpr "42")) "lit"

    putStrLn "== splicing =="
    let e1 = [expr| 1 |]
        e2 = [expr| x |]
    check "splice into operator operands"
        [expr| $e1 + $e2 |] (parseExpr "1 + x")
    check "splice into application"
        [expr| $e2 $e1 |] (parseExpr "x 1")
    let x1 = IdN "z"
    check "binder metavar in construction"
        [expr| fn $x1 -> $x1 + 1 |] (parseExpr "fn z -> z + 1")

    putStrLn "== pattern + splice round trip =="
    let getBody e = case e of
            [expr| fn $x1 -> $e1 |] -> e1
            _                       -> error "not a lambda"
    check "destructure a lambda binder and body"
        (getBody (parseExpr "fn v -> v * v")) (parseExpr "v * v")

    putStrLn "== SYB substitution with QQ patterns =="
    check "subst replaces free occurrences"
        (subst (IdN "v") (LitI 9) (parseExpr "v + w")) (parseExpr "9 + w")
    check "subst leaves binders alone"
        (subst (IdN "v") (LitI 9) (parseExpr "fn v -> v"))
        (parseExpr "fn v -> 9")  -- capture-naive by design: documents behavior

    putStrLn "== call-by-value evaluation =="
    check "arithmetic and comparison"
        (eval (parseExpr "if 2 + 3 == 5 then 42 else 0")) (LitI 42)
    check "beta reduction"
        (eval (parseExpr "(fn y -> y * y) (3 + 1)")) (LitI 16)
    check "let and higher-order functions"
        (eval (parseExpr
            "let compose = fn f -> fn g -> fn v -> f (g v) in \
            \let addone = fn y -> y + 1 in compose addone addone 5"))
        (LitI 7)

    n <- readIORef failures
    if n == 0
        then putStrLn "\nAll lc spike tests passed."
        else do
            putStrLn $ "\n" ++ show n ++ " lc spike test(s) FAILED."
            exitFailure
