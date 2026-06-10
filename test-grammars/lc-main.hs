{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- "Write You a Haskell" on RTK, chapters 3-4: an interpreter for the
-- untyped lambda calculus (with the literals, conditionals and operator
-- ladder of the tutorial's later Poly language) whose frontend is fully
-- generated from lc.pg.
--
-- Everything that inspects or builds syntax is written with quasi-quotes
-- against concrete syntax: the call-by-value evaluator, free variables,
-- capture-avoiding substitution and the pretty-printer. The only raw
-- generated constructors used are integer literals (Int fields cannot be
-- antiquoted) and identifiers, wrapped in pattern synonyms below.
--
-- Run modes:
--   lc-main         run the test suite (make test-lc)
--   lc-main repl    interactive read-eval-print loop (make repl-lc)

import Control.Exception (SomeException, evaluate, try)
import Data.Generics (gmapQ, gmapT, mkQ, mkT)
import Data.IORef
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)

import LcLexer
import LcParser
import LcQQ

-- Explicitly bidirectional: matching ignores the constructors' RtkPos
-- field, construction supplies rtkNoPos.
pattern LitI :: Int -> Expr
pattern LitI n <- Ctr__Expr__0 _ n
  where LitI n = Ctr__Expr__0 rtkNoPos n

pattern IdN :: String -> Id
pattern IdN s <- Ctr__Id__0 _ s
  where IdN s = Ctr__Id__0 rtkNoPos s

unId :: Id -> String
unId (IdN s)     = s
unId (Anti_Id s) = '$' : s
unId other       = show other

mkVar :: Id -> Expr
mkVar x1 = [expr| $x1 |]

-- Unwrapping the start symbol is itself a QQ pattern on the start rule.
unLc :: Lc -> Expr
unLc [lc| $e |] = e
unLc other      = error $ "unexpected start wrapper: " ++ show other

-- Prelude.id: LcQQ also exports an 'id' quasi-quoter for the Id type
parseExpr :: String -> Expr
parseExpr src =
    unLc $ either errorWithoutStackTrace Prelude.id (scanTokens src >>= parseLc)

-- --------------------------------------------------------------------
-- Values and call-by-value evaluation in an environment (the design of
-- the tutorial's chapter-4 Eval.hs: lambdas evaluate to closures).
-- --------------------------------------------------------------------

type Env = M.Map Id Value

data Value = VInt Int
           | VBool Bool
           | VClosure Id Expr Env
           deriving Eq

instance Show Value where
    show (VInt n)          = show n
    show (VBool True)      = "true"
    show (VBool False)     = "false"
    show (VClosure x e _)  = "<<closure>> \\" ++ unId x ++ " -> " ++ ppr 0 e

eval :: Env -> Expr -> Value
eval env [expr| \ $x1 -> $e1 |] = VClosure x1 e1 env
eval env [expr| let $x1 = $e1 in $e2 |] =
    eval (M.insert x1 (eval env e1) env) e2
eval env [expr| if $e1 then $e2 else $e3 |] =
    case eval env e1 of
        VBool True  -> eval env e2
        VBool False -> eval env e3
        v           -> error $ "if condition is not a boolean: " ++ show v
eval env [expr| $e1 == $e2 |] =
    case (eval env e1, eval env e2) of
        (VInt a,  VInt b)  -> VBool (a == b)
        (VBool a, VBool b) -> VBool (a == b)
        (v1, v2) -> error $ "cannot compare " ++ show v1 ++ " == " ++ show v2
eval env [expr| $e1 + $e2 |] = vArith "+" (+) (eval env e1) (eval env e2)
eval env [expr| $e1 - $e2 |] = vArith "-" (-) (eval env e1) (eval env e2)
eval env [expr| $e1 * $e2 |] = vArith "*" (*) (eval env e1) (eval env e2)
eval env [expr| $e1 $e2 |] =
    case eval env e1 of
        VClosure x body cenv -> eval (M.insert x (eval env e2) cenv) body
        v -> error $ "cannot apply non-function: " ++ show v
eval env [expr| $x1 |] =
    case M.lookup x1 env of
        Just v  -> v
        Nothing -> error $ "unbound variable: " ++ unId x1
eval _ [expr| true |]  = VBool True
eval _ [expr| false |] = VBool False
eval _ (LitI n)        = VInt n
eval _ e               = error $ "cannot evaluate: " ++ show e

vArith :: String -> (Int -> Int -> Int) -> Value -> Value -> Value
vArith _  op (VInt a) (VInt b) = VInt (a `op` b)
vArith nm _  v1       v2       =
    error $ "cannot compute " ++ show v1 ++ " " ++ nm ++ " " ++ show v2

-- --------------------------------------------------------------------
-- Free variables and capture-avoiding substitution. Binder cases are
-- QQ patterns; non-binding nodes recurse generically with SYB, so the
-- functions need no case per operator.
-- --------------------------------------------------------------------

freeVars :: Expr -> S.Set Id
freeVars [expr| \ $x1 -> $e1 |]          = S.delete x1 (freeVars e1)
freeVars [expr| let $x1 = $e1 in $e2 |]  =
    freeVars e1 `S.union` S.delete x1 (freeVars e2)
freeVars [expr| $x1 |]                   = S.singleton x1
freeVars e = S.unions (gmapQ (mkQ S.empty freeVars) e)

freshFor :: S.Set Id -> Id -> Id
freshFor taken x0 =
    head [ x | k <- [0 :: Int ..]
             , let x = IdN (unId x0 ++ show k)
             , not (x `S.member` taken) ]

-- subst x v e: replace free occurrences of x by v in e, renaming binders
-- that would capture free variables of v.
subst :: Id -> Expr -> Expr -> Expr
subst x v e0 = case e0 of
    [expr| $x1 |]
        | x1 == x   -> v
        | otherwise -> e0
    [expr| \ $x1 -> $e1 |]
        | x1 == x   -> e0
        | x1 `S.member` fvV ->
            let x2 = freshFor (S.unions [fvV, freeVars e1, S.singleton x]) x1
                e2 = subst x v (subst x1 (mkVar x2) e1)
            in [expr| \ $x2 -> $e2 |]
        | otherwise -> let e2 = subst x v e1 in [expr| \ $x1 -> $e2 |]
    [expr| let $x1 = $e1 in $e2 |] ->
        let e3 = subst x v e1 in
        if x1 == x
          then [expr| let $x1 = $e3 in $e2 |]
          else if x1 `S.member` fvV
            then let x2 = freshFor (S.unions [fvV, freeVars e2, S.singleton x]) x1
                     e4 = subst x v (subst x1 (mkVar x2) e2)
                 in [expr| let $x2 = $e3 in $e4 |]
            else let e4 = subst x v e2 in [expr| let $x1 = $e3 in $e4 |]
    _ -> gmapT (mkT (subst x v)) e0
  where fvV = freeVars v

-- --------------------------------------------------------------------
-- Pretty-printer with minimal parentheses, mirroring the grammar's
-- precedence ladder (0 = lam/let/if, 1 = ==, 2 = + -, 3 = *, 4 = app).
-- --------------------------------------------------------------------

ppr :: Int -> Expr -> String
ppr d e0 = case e0 of
    [expr| \ $x1 -> $e1 |] ->
        paren (d > 0) $ "\\" ++ unId x1 ++ " -> " ++ ppr 0 e1
    [expr| let $x1 = $e1 in $e2 |] ->
        paren (d > 0) $ "let " ++ unId x1 ++ " = " ++ ppr 0 e1
                          ++ " in " ++ ppr 0 e2
    [expr| if $e1 then $e2 else $e3 |] ->
        paren (d > 0) $ "if " ++ ppr 0 e1 ++ " then " ++ ppr 0 e2
                          ++ " else " ++ ppr 0 e3
    [expr| $e1 == $e2 |] -> paren (d > 1) $ ppr 1 e1 ++ " == " ++ ppr 2 e2
    [expr| $e1 + $e2 |]  -> paren (d > 2) $ ppr 2 e1 ++ " + "  ++ ppr 3 e2
    [expr| $e1 - $e2 |]  -> paren (d > 2) $ ppr 2 e1 ++ " - "  ++ ppr 3 e2
    [expr| $e1 * $e2 |]  -> paren (d > 3) $ ppr 3 e1 ++ " * "  ++ ppr 4 e2
    [expr| $e1 $e2 |]    -> paren (d > 4) $ ppr 4 e1 ++ " "    ++ ppr 5 e2
    [expr| true |]       -> "true"
    [expr| false |]      -> "false"
    [expr| $x1 |]        -> unId x1
    LitI n               -> show n
    other                -> show other  -- Anti_ nodes (never built at runtime)
  where paren True  s = "(" ++ s ++ ")"
        paren False s = s

-- --------------------------------------------------------------------
-- QQ pattern classification across all ladder levels; the binary
-- operator cases are exactly what fails for the Java grammar.
-- --------------------------------------------------------------------

describe :: Expr -> String
describe [expr| \ $x1 -> $e1 |]             = "lam"
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
-- REPL
-- --------------------------------------------------------------------

repl :: IO ()
repl = do
    putStrLn "lc - untyped lambda calculus on RTK (:q to quit)"
    loop
  where
    loop = do
        putStr "lc> "
        hFlush stdout
        end <- isEOF
        if end then putStrLn "" else do
            line <- getLine
            case words line of
                []     -> loop
                [":q"] -> return ()
                _      -> do
                    result <- try (evaluate (forceShow line))
                    case result of
                        Left e    -> putStrLn $ takeWhile (/= '\n') $
                                       show (e :: SomeException)
                        Right out -> putStrLn out
                    loop
    forceShow line =
        let out = show (eval M.empty (parseExpr line))
        in length out `seq` out

-- --------------------------------------------------------------------
-- Test suite
-- --------------------------------------------------------------------

runTests :: IO ()
runTests = do
    failures <- newIORef (0 :: Int)
    let check :: (Eq a, Show a) => String -> a -> a -> IO ()
        check label actual expected
            | actual == expected = putStrLn $ "PASS  " ++ label
            | otherwise = do
                putStrLn $ "FAIL  " ++ label
                putStrLn $ "      expected: " ++ show expected
                putStrLn $ "      actual:   " ++ show actual
                modifyIORef failures (+ 1)
        evalStr = eval M.empty . parseExpr

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
        [expr| \ x -> x + 1 |] (parseExpr "\\x -> x + 1")
    check "let/if quote = parsed let/if"
        [expr| let y = 1 in if y == 1 then true else false |]
        (parseExpr "let y = 1 in if y == 1 then true else false")

    putStrLn "== pattern quotes at every precedence level =="
    check "describe lam" (describe (parseExpr "\\x -> x")) "lam"
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
        [expr| \ $x1 -> $x1 + 1 |] (parseExpr "\\z -> z + 1")

    putStrLn "== pattern + splice round trip =="
    let getBody e = case e of
            [expr| \ $x1 -> $e1 |] -> e1
            _                      -> error "not a lambda"
    check "destructure a lambda binder and body"
        (getBody (parseExpr "\\v -> v * v")) (parseExpr "v * v")

    putStrLn "== free variables =="
    check "freeVars of open term"
        (freeVars (parseExpr "\\x -> x + y * z"))
        (S.fromList [IdN "y", IdN "z"])
    check "let binds only its body"
        (freeVars (parseExpr "let x = x + 1 in x"))
        (S.fromList [IdN "x"])

    putStrLn "== capture-avoiding substitution =="
    check "subst replaces free occurrences"
        (subst (IdN "v") (LitI 9) (parseExpr "v + w")) (parseExpr "9 + w")
    check "subst stops at shadowing binders"
        (subst (IdN "v") (LitI 9) (parseExpr "\\v -> v"))
        (parseExpr "\\v -> v")
    check "subst renames a capturing lambda binder"
        (subst (IdN "y") (parseExpr "x") (parseExpr "\\x -> y"))
        (parseExpr "\\x0 -> x")
    check "subst renames a capturing let binder"
        (subst (IdN "y") (parseExpr "x") (parseExpr "let x = 1 in y + x"))
        (parseExpr "let x0 = 1 in x + x0")

    putStrLn "== pretty-printing with minimal parentheses =="
    check "ppr drops redundant parens"
        (ppr 0 (parseExpr "1 + (2 * 3)")) "1 + 2 * 3"
    check "ppr keeps necessary parens"
        (ppr 0 (parseExpr "(1 + 2) * 3")) "(1 + 2) * 3"
    check "ppr of lambda application"
        (ppr 0 (parseExpr "(\\x -> x) (f 1)")) "(\\x -> x) (f 1)"
    let roundtrip s = parseExpr (ppr 0 (parseExpr s)) == parseExpr s
    check "ppr/parse round trip"
        (all roundtrip
            [ "\\x -> x + 1"
            , "f x y == g (h 1) * 2 - 3"
            , "let f = \\y -> y in if f 1 == 1 then f 2 else 0"
            , "1 - (2 - 3) * 4"
            ])
        True

    putStrLn "== call-by-value evaluation with closures =="
    check "arithmetic and comparison"
        (evalStr "if 2 + 3 == 5 then 42 else 0") (VInt 42)
    check "beta reduction"
        (evalStr "(\\y -> y * y) (3 + 1)") (VInt 16)
    check "higher-order functions"
        (evalStr "let compose = \\f -> \\g -> \\v -> f (g v) in \
                 \let addone = \\y -> y + 1 in compose addone addone 5")
        (VInt 7)
    check "church numeral two"
        (evalStr "(\\s -> \\z -> s (s z)) (\\n -> n + 1) 0") (VInt 2)
    check "let shadowing"
        (evalStr "let x = 1 in let x = 2 in x") (VInt 2)
    check "closures capture statically"
        (evalStr "let n = 1 in let f = \\m -> n + m in let n = 100 in f 1")
        (VInt 2)
    check "closure value printing"
        (show (evalStr "let n = 3 in \\m -> n + m"))
        "<<closure>> \\m -> n + m"

    n <- readIORef failures
    if n == 0
        then putStrLn "\nAll lc tests passed."
        else do
            putStrLn $ "\n" ++ show n ++ " lc test(s) FAILED."
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["repl"] -> repl
        _        -> runTests
