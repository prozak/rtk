{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- "Write You a Haskell" on RTK, chapters 5-6: the simply typed lambda
-- calculus, frontend generated from stlc.pg.
--
-- Chapter 5: a typechecker written with quasi-quote patterns over BOTH
-- generated nonterminal families - expressions and types. Result types
-- are built by splicing ([ty| $t1 -> $t2 |]), and the application rule
-- destructures arrow types with a [ty| ... |] pattern.
--
-- Chapter 6: one closure evaluator parameterized by evaluation strategy.
-- Call-by-value forces the argument before extending the environment;
-- call-by-name just inserts the unforced result, borrowing laziness from
-- the host the same way the tutorial's lazy interpreter does. The
-- ill-typed-but-convergent test below shows the difference, and that the
-- typechecker rejects exactly the term that gets CBV stuck.
--
-- Run modes:
--   stlc-main         run the test suite (make test-stlc)
--   stlc-main repl    typecheck-then-eval loop (make repl-stlc)

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (unless)
import Data.IORef
import qualified Data.Map.Strict as M
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)

import StlcLexer
import StlcParser
import StlcQQ

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

unStlc :: Stlc -> Expr
unStlc [stlc| $e |] = e
unStlc other        = error $ "unexpected start wrapper: " ++ show other

-- Prelude.id: StlcQQ also exports an 'id' quasi-quoter for the Id type
parseExpr :: String -> Expr
parseExpr src =
    unStlc $ either errorWithoutStackTrace Prelude.id (scanTokens src >>= parseStlc)

-- --------------------------------------------------------------------
-- Chapter 5: type checking
-- --------------------------------------------------------------------

type TEnv = M.Map Id Ty

check :: TEnv -> Expr -> Either String Ty
check env e0 = case e0 of
    [expr| \ $x1 : $t1 . $e1 |] -> do
        t2 <- check (M.insert x1 t1 env) e1
        return [ty| $t1 -> $t2 |]
    [expr| let $x1 = $e1 in $e2 |] -> do
        t1 <- check env e1
        check (M.insert x1 t1 env) e2
    [expr| if $e1 then $e2 else $e3 |] -> do
        expect [ty| Bool |] "if condition" e1
        t2 <- check env e2
        t3 <- check env e3
        unless (t2 == t3) $ Left $
            "branches of if have different types: "
            ++ pprTy 0 t2 ++ " and " ++ pprTy 0 t3
        return t2
    [expr| $e1 == $e2 |] -> do
        t1 <- check env e1
        t2 <- check env e2
        unless (t1 == t2 && elem t1 [[ty| Int |], [ty| Bool |]]) $ Left $
            "cannot compare " ++ pprTy 0 t1 ++ " == " ++ pprTy 0 t2
        return [ty| Bool |]
    [expr| $e1 + $e2 |] -> intOp "+" e1 e2
    [expr| $e1 - $e2 |] -> intOp "-" e1 e2
    [expr| $e1 * $e2 |] -> intOp "*" e1 e2
    [expr| $e1 $e2 |] -> do
        t1 <- check env e1
        t2 <- check env e2
        case t1 of
            [ty| $t3 -> $t4 |]
                | t3 == t2  -> return t4
                | otherwise -> Left $ "argument type mismatch: expected "
                                 ++ pprTy 0 t3 ++ ", got " ++ pprTy 0 t2
            _ -> Left $ "cannot apply a value of type " ++ pprTy 0 t1
    [expr| $x1 |] ->
        maybe (Left $ "unbound variable: " ++ unId x1) Right (M.lookup x1 env)
    [expr| true |]  -> return [ty| Bool |]
    [expr| false |] -> return [ty| Bool |]
    LitI _          -> return [ty| Int |]
    _               -> Left $ "cannot type: " ++ show e0
  where
    expect t what e = do
        t' <- check env e
        unless (t' == t) $ Left $
            what ++ " must have type " ++ pprTy 0 t ++ ", got " ++ pprTy 0 t'
    intOp nm e1 e2 = do
        expect [ty| Int |] ("left operand of " ++ nm) e1
        expect [ty| Int |] ("right operand of " ++ nm) e2
        return [ty| Int |]

-- --------------------------------------------------------------------
-- Chapter 6: one evaluator, two strategies
-- --------------------------------------------------------------------

data Strategy = CBV | CBN deriving (Eq, Show)

type Env = M.Map Id Value

data Value = VInt Int
           | VBool Bool
           | VClosure Id Expr Env
           deriving Eq

instance Show Value where
    show (VInt n)         = show n
    show (VBool True)     = "true"
    show (VBool False)    = "false"
    show (VClosure x e _) = "<<closure>> \\" ++ unId x ++ " . " ++ ppr 0 e

-- strictly: under CBV the bound value is forced to WHNF before
-- evaluation proceeds into the body; under CBN it stays an unevaluated
-- host thunk that only a variable lookup will force.
strictly :: Strategy -> Value -> a -> a
strictly CBV v k = v `seq` k
strictly CBN _ k = k

eval :: Strategy -> Env -> Expr -> Value
eval s env e0 = case e0 of
    [expr| \ $x1 : $t1 . $e1 |] -> VClosure x1 e1 env
    [expr| let $x1 = $e1 in $e2 |] ->
        let v = eval s env e1
        in strictly s v $ eval s (M.insert x1 v env) e2
    [expr| if $e1 then $e2 else $e3 |] ->
        case eval s env e1 of
            VBool True  -> eval s env e2
            VBool False -> eval s env e3
            v           -> error $ "if condition is not a boolean: " ++ show v
    [expr| $e1 == $e2 |] ->
        case (eval s env e1, eval s env e2) of
            (VInt a,  VInt b)  -> VBool (a == b)
            (VBool a, VBool b) -> VBool (a == b)
            (v1, v2) -> error $ "cannot compare " ++ show v1 ++ " == " ++ show v2
    [expr| $e1 + $e2 |] -> vArith "+" (+) (eval s env e1) (eval s env e2)
    [expr| $e1 - $e2 |] -> vArith "-" (-) (eval s env e1) (eval s env e2)
    [expr| $e1 * $e2 |] -> vArith "*" (*) (eval s env e1) (eval s env e2)
    [expr| $e1 $e2 |] ->
        case eval s env e1 of
            VClosure x body cenv ->
                let arg = eval s env e2
                in strictly s arg $ eval s (M.insert x arg cenv) body
            v -> error $ "cannot apply non-function: " ++ show v
    [expr| $x1 |] ->
        case M.lookup x1 env of
            Just v  -> v
            Nothing -> error $ "unbound variable: " ++ unId x1
    [expr| true |]  -> VBool True
    [expr| false |] -> VBool False
    LitI n          -> VInt n
    _               -> error $ "cannot evaluate: " ++ show e0

vArith :: String -> (Int -> Int -> Int) -> Value -> Value -> Value
vArith _  op (VInt a) (VInt b) = VInt (a `op` b)
vArith nm _  v1       v2       =
    error $ "cannot compute " ++ show v1 ++ " " ++ nm ++ " " ++ show v2

-- --------------------------------------------------------------------
-- Pretty-printers (minimal parentheses; the type arrow associates right)
-- --------------------------------------------------------------------

pprTy :: Int -> Ty -> String
pprTy d [ty| $t1 -> $t2 |]
    | d > 0     = "(" ++ s ++ ")"
    | otherwise = s
  where s = pprTy 1 t1 ++ " -> " ++ pprTy 0 t2
pprTy _ [ty| Int |]  = "Int"
pprTy _ [ty| Bool |] = "Bool"
pprTy _ other        = show other

ppr :: Int -> Expr -> String
ppr d e0 = case e0 of
    [expr| \ $x1 : $t1 . $e1 |] ->
        paren (d > 0) $ "\\" ++ unId x1 ++ " : " ++ pprTy 0 t1
                          ++ " . " ++ ppr 0 e1
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
    other                -> show other
  where paren True  s = "(" ++ s ++ ")"
        paren False s = s

-- --------------------------------------------------------------------
-- REPL: typecheck, then evaluate
-- --------------------------------------------------------------------

repl :: IO ()
repl = do
    putStrLn "stlc - simply typed lambda calculus on RTK (:q to quit)"
    loop
  where
    loop = do
        putStr "stlc> "
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
        let e   = parseExpr line
            out = case check M.empty e of
                    Left err -> "type error: " ++ err
                    Right t  -> show (eval CBV M.empty e)
                                  ++ " : " ++ pprTy 0 t
        in length out `seq` out

-- --------------------------------------------------------------------
-- Test suite
-- --------------------------------------------------------------------

runTests :: IO ()
runTests = do
    failures <- newIORef (0 :: Int)
    let check' :: (Eq a, Show a) => String -> a -> a -> IO ()
        check' label actual expected
            | actual == expected = putStrLn $ "PASS  " ++ label
            | otherwise = do
                putStrLn $ "FAIL  " ++ label
                putStrLn $ "      expected: " ++ show expected
                putStrLn $ "      actual:   " ++ show actual
                modifyIORef failures (+ 1)
        checkStr  = check M.empty . parseExpr
        typeOf    = fmap (pprTy 0) . checkStr
        evalStr s = eval CBV M.empty (parseExpr s)
        evalCatch strat s = do
            r <- try (evaluate (eval strat M.empty (parseExpr s)))
            return $ case r of
                Left err -> "stuck: " ++ takeWhile (/= '\n')
                              (show (err :: SomeException))
                Right v  -> show v

    putStrLn "== type QQ: construction and patterns =="
    check' "type arrow associates right"
        [ty| Int -> Int -> Bool |] [ty| Int -> (Int -> Bool) |]
    check' "type pattern destructures arrows"
        (case [ty| (Int -> Bool) -> Int |] of
            [ty| $t1 -> $t2 |] -> (pprTy 0 t1, pprTy 0 t2)
            _                  -> ("?", "?"))
        ("Int -> Bool", "Int")
    let t1 = [ty| Int |]
    check' "type splice"
        [ty| $t1 -> Bool |] [ty| Int -> Bool |]

    putStrLn "== chapter 5: the typechecker accepts =="
    check' "identity"
        (typeOf "\\x : Int . x") (Right "Int -> Int")
    check' "higher-order argument"
        (typeOf "\\f : Int -> Int . f 1 + 1") (Right "(Int -> Int) -> Int")
    check' "application"
        (typeOf "(\\x : Int . x + 1) 41") (Right "Int")
    check' "let and if"
        (typeOf "let b = 1 == 2 in if b then 1 else 0") (Right "Int")
    check' "nested annotation"
        (typeOf "\\f : Int -> Int -> Bool . \\x : Int . f x x")
        (Right "(Int -> Int -> Bool) -> Int -> Bool")

    putStrLn "== chapter 5: the typechecker rejects =="
    check' "adding a boolean"
        (checkStr "1 + true")
        (Left "right operand of + must have type Int, got Bool")
    check' "applying a non-function"
        (checkStr "1 2")
        (Left "cannot apply a value of type Int")
    check' "argument mismatch"
        (checkStr "(\\x : Int . x) true")
        (Left "argument type mismatch: expected Int, got Bool")
    check' "non-boolean condition"
        (checkStr "if 1 then 2 else 3")
        (Left "if condition must have type Bool, got Int")
    check' "branch mismatch"
        (checkStr "if true then 1 else false")
        (Left "branches of if have different types: Int and Bool")
    check' "unbound variable"
        (checkStr "x + 1") (Left "unbound variable: x")

    putStrLn "== evaluation (call-by-value) =="
    check' "arithmetic under closures"
        (evalStr "(\\x : Int . x * x) (3 + 1)") (VInt 16)
    check' "higher-order"
        (evalStr "let twice = \\f : Int -> Int . \\x : Int . f (f x) in \
                 \twice (\\n : Int . n + 1) 0")
        (VInt 2)
    check' "static scoping"
        (evalStr "let n = 1 in let f = \\m : Int . n + m in \
                 \let n = 100 in f 1")
        (VInt 2)

    putStrLn "== chapter 6: strategies differ exactly on ill-typed terms =="
    let discard = "(\\x : Int . 2) (1 + true)"
    cbv <- evalCatch CBV discard
    cbn <- evalCatch CBN discard
    check' "call-by-value gets stuck on the unused bad argument"
        cbv "stuck: cannot compute 1 + true"
    check' "call-by-name never looks at it"
        cbn "2"
    check' "and the typechecker rejects that term anyway"
        (either (const "rejected") (const "accepted") (checkStr discard))
        "rejected"
    agreeV <- evalCatch CBV "(\\x : Int . x + 1) 41"
    agreeN <- evalCatch CBN "(\\x : Int . x + 1) 41"
    check' "strategies agree on well-typed terms" agreeV agreeN

    putStrLn "== pretty-printing =="
    check' "expression round trip"
        (parseExpr (ppr 0 (parseExpr
            "let f = \\g : (Int -> Int) -> Int . g (\\n : Int . n) in f")))
        (parseExpr
            "let f = \\g : (Int -> Int) -> Int . g (\\n : Int . n) in f")
    check' "type printing keeps needed parens"
        (pprTy 0 [ty| (Int -> Bool) -> Int -> Bool |])
        "(Int -> Bool) -> Int -> Bool"

    n <- readIORef failures
    if n == 0
        then putStrLn "\nAll stlc tests passed."
        else do
            putStrLn $ "\n" ++ show n ++ " stlc test(s) FAILED."
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["repl"] -> repl
        _        -> runTests
