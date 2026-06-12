{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- "Write You a Haskell" on RTK, chapter 7: Poly, an ML-flavored language
-- with Hindley-Milner type inference. Frontend generated from poly.pg.
--
-- The pipeline mirrors the tutorial's poly interpreter:
--
--   parse            - generated parser, programs are lists of decls
--   desugar          - QQ rewrites: multi-parameter lambdas curry into
--                      single-parameter ones, 'let rec' becomes 'fix',
--                      'let f x = e' becomes 'let f = \x -> e'
--   infer            - algorithm W over the generated AST with QQ
--                      patterns: unification, occurs check, and
--                      let-generalization (types themselves are a small
--                      hand-written Haskell datatype - Poly's surface
--                      syntax has no annotations to put in the grammar)
--   eval             - call-by-value closures; 'fix' ties a lazy knot
--
-- Run modes:
--   poly-main         run the test suite (make test-poly)
--   poly-main repl    stateful infer-then-eval loop (make repl-poly)

import Control.Exception (SomeException, evaluate, try)
-- mtl >= 2.3 no longer re-exports Control.Monad from the transformer modules
import Control.Monad (foldM)
import Control.Monad.Except
import Control.Monad.State
import Data.Generics (gmapT, mkT)
import Data.IORef
import Data.List (nub)
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)

import PolyLexer
import PolyParser
import PolyQQ

-- Explicitly bidirectional: matching ignores the constructors' RtkPos
-- field, construction supplies rtkNoPos.
pattern LitI :: Int -> Expr
pattern LitI n <- Ctr__Expr__0 _ n
  where LitI n = Ctr__Expr__0 rtkNoPos n

pattern IdN :: String -> Id
pattern IdN s <- Ctr__Id__0 _ s
  where IdN s = Ctr__Id__0 rtkNoPos s

pattern ParamN :: Id -> Param
pattern ParamN x <- Ctr__Param__0 _ x
  where ParamN x = Ctr__Param__0 rtkNoPos x

unId :: Id -> String
unId (IdN s)     = s
unId (Anti_Id s) = '$' : s
unId other       = show other

paramId :: Param -> Id
paramId (ParamN x) = x
paramId other      = error $ "unexpected param: " ++ show other

unPoly :: Poly -> [Decl]
unPoly [poly| $d |] = d
unPoly other        = error $ "unexpected start wrapper: " ++ show other

-- Prelude.id: PolyQQ also exports an 'id' quasi-quoter for the Id type
parseProgram :: String -> [Decl]
parseProgram src =
    unPoly $ either errorWithoutStackTrace Prelude.id (scanTokens src >>= parsePoly)

parseExpr :: String -> Expr
parseExpr src = case parseProgram (src ++ " ;") of
    [ [decl| $e1 ; |] ] -> e1
    _                   -> error $ "not a single expression: " ++ src

-- --------------------------------------------------------------------
-- Desugaring to the core language: every binder form becomes a
-- single-parameter lambda, 'let rec' becomes 'fix'. Each rule is a
-- quasi-quote rewrite; everything else recurses generically.
-- --------------------------------------------------------------------

lamWrap :: [Param] -> Expr -> Expr
lamWrap ps body = foldr one body ps
  where one q e1 = let p1 = [q] in [expr| \ $p1 -> $e1 |]

fixWrap :: Id -> Expr -> Expr
fixWrap x1 e1 = let p1 = [ParamN x1] in [expr| fix (\ $p1 -> $e1) |]

desugarE :: Expr -> Expr
desugarE e0 = case e0 of
    [expr| \ $p1 -> $e1 |] -> lamWrap p1 (desugarE e1)
    [expr| let rec $x1 = $e1 in $e2 |] ->
        let e3 = fixWrap x1 (desugarE e1)
            e4 = desugarE e2
        in [expr| let $x1 = $e3 in $e4 |]
    _ -> gmapT (mkT desugarE) e0

desugarDecl :: Decl -> Decl
desugarDecl [decl| let rec $x1 = $e1 ; |] =
    let e2 = fixWrap x1 (desugarE e1) in [decl| let $x1 = $e2 ; |]
desugarDecl [decl| let rec $x1 $p1 = $e1 ; |] =
    let e2 = fixWrap x1 (desugarE (lamWrap p1 e1)) in [decl| let $x1 = $e2 ; |]
desugarDecl [decl| let $x1 $p1 = $e1 ; |] =
    let e2 = desugarE (lamWrap p1 e1) in [decl| let $x1 = $e2 ; |]
desugarDecl [decl| let $x1 = $e1 ; |] =
    let e2 = desugarE e1 in [decl| let $x1 = $e2 ; |]
desugarDecl [decl| $e1 ; |] =
    let e2 = desugarE e1 in [decl| $e2 ; |]
desugarDecl other = other

-- --------------------------------------------------------------------
-- Types and schemes (hand-written: Poly's surface syntax has no type
-- annotations, so types never appear in the grammar)
-- --------------------------------------------------------------------

data Type = TVar String
          | TInt
          | TBool
          | TArr Type Type
          deriving (Eq, Ord, Show)

data Scheme = Forall [String] Type
              deriving (Eq, Show)

pprType :: Int -> Type -> String
pprType d (TArr t1 t2)
    | d > 0     = "(" ++ s ++ ")"
    | otherwise = s
  where s = pprType 1 t1 ++ " -> " ++ pprType 0 t2
pprType _ TInt      = "Int"
pprType _ TBool     = "Bool"
pprType _ (TVar v)  = v

-- Print with variables renamed a, b, c ... by order of appearance,
-- like the tutorial's ppscheme.
pprScheme :: Scheme -> String
pprScheme (Forall _ t) = pprType 0 (rename t)
  where
    names   = map (:[]) ['a' .. 'z'] ++ [ 'a' : show k | k <- [1 :: Int ..] ]
    table   = M.fromList (zip (nub (collect t)) names)
    collect (TVar v)     = [v]
    collect (TArr a b)   = collect a ++ collect b
    collect _            = []
    rename (TVar v)      = TVar (M.findWithDefault v v table)
    rename (TArr a b)    = TArr (rename a) (rename b)
    rename other         = other

-- --------------------------------------------------------------------
-- Hindley-Milner inference: algorithm W (chapter 7's Infer.hs)
-- --------------------------------------------------------------------

type Subst   = M.Map String Type
type TypeEnv = M.Map Id Scheme
type Infer a = ExceptT String (State Int) a

fresh :: Infer Type
fresh = do
    n <- get
    put (n + 1)
    return $ TVar ("t" ++ show n)

applyT :: Subst -> Type -> Type
applyT s t@(TVar v)   = M.findWithDefault t v s
applyT s (TArr t1 t2) = TArr (applyT s t1) (applyT s t2)
applyT _ t            = t

ftvT :: Type -> S.Set String
ftvT (TVar v)     = S.singleton v
ftvT (TArr t1 t2) = ftvT t1 `S.union` ftvT t2
ftvT _            = S.empty

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Forall vs t) = Forall vs (applyT (foldr M.delete s vs) t)

ftvScheme :: Scheme -> S.Set String
ftvScheme (Forall vs t) = ftvT t `S.difference` S.fromList vs

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s = M.map (applyScheme s)

ftvEnv :: TypeEnv -> S.Set String
ftvEnv = S.unions . map ftvScheme . M.elems

-- composeSubst s1 s2 applies s2 first, then s1
composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (applyT s1) s2 `M.union` s1

unify :: Type -> Type -> Infer Subst
unify (TArr a1 b1) (TArr a2 b2) = do
    s1 <- unify a1 a2
    s2 <- unify (applyT s1 b1) (applyT s1 b2)
    return (s2 `composeSubst` s1)
unify (TVar v) t = bindVar v t
unify t (TVar v) = bindVar v t
unify t1 t2
    | t1 == t2  = return M.empty
    | otherwise = throwError $
        "cannot unify " ++ pprType 0 t1 ++ " with " ++ pprType 0 t2

bindVar :: String -> Type -> Infer Subst
bindVar v t
    | t == TVar v          = return M.empty
    | v `S.member` ftvT t  = throwError $
        "infinite type: " ++ v ++ " = " ++ pprType 0 t
    | otherwise            = return (M.singleton v t)

instantiate :: Scheme -> Infer Type
instantiate (Forall vs t) = do
    vs' <- mapM (const fresh) vs
    return $ applyT (M.fromList (zip vs vs')) t

generalize :: TypeEnv -> Type -> Scheme
generalize env t =
    Forall (S.toList (ftvT t `S.difference` ftvEnv env)) t

infer :: TypeEnv -> Expr -> Infer (Subst, Type)
infer env e0 = case e0 of
    [expr| \ $p1 -> $e1 |] -> case p1 of
        [q1] -> do
            tv <- fresh
            (s1, t1) <- infer (M.insert (paramId q1) (Forall [] tv) env) e1
            return (s1, TArr (applyT s1 tv) t1)
        _ -> throwError "internal error: lambda not desugared to one parameter"
    [expr| let $x1 = $e1 in $e2 |] -> do
        (s1, t1) <- infer env e1
        let env1 = applyEnv s1 env
            sc   = generalize env1 (applyT s1 t1)
        (s2, t2) <- infer (M.insert x1 sc env1) e2
        return (s2 `composeSubst` s1, t2)
    [expr| if $e1 then $e2 else $e3 |] -> do
        (s1, t1) <- infer env e1
        s2 <- unify t1 TBool
        let env2 = applyEnv (s2 `composeSubst` s1) env
        (s3, t3) <- infer env2 e2
        (s4, t4) <- infer (applyEnv s3 env2) e3
        s5 <- unify (applyT s4 t3) t4
        return ( foldr1 composeSubst [s5, s4, s3, s2, s1]
               , applyT s5 t4 )
    [expr| fix $e1 |] -> do
        (s1, t1) <- infer env e1
        tv <- fresh
        s2 <- unify t1 (TArr tv tv)
        return (s2 `composeSubst` s1, applyT s2 tv)
    [expr| $e1 == $e2 |] -> binOp env e1 e2 TBool
    [expr| $e1 + $e2 |]  -> binOp env e1 e2 TInt
    [expr| $e1 - $e2 |]  -> binOp env e1 e2 TInt
    [expr| $e1 * $e2 |]  -> binOp env e1 e2 TInt
    [expr| $e1 $e2 |] -> do
        (s1, t1) <- infer env e1
        (s2, t2) <- infer (applyEnv s1 env) e2
        tv <- fresh
        s3 <- unify (applyT s2 t1) (TArr t2 tv)
        return (foldr1 composeSubst [s3, s2, s1], applyT s3 tv)
    [expr| $x1 |] -> case M.lookup x1 env of
        Nothing -> throwError $ "unbound variable: " ++ unId x1
        Just sc -> do
            t <- instantiate sc
            return (M.empty, t)
    [expr| true |]  -> return (M.empty, TBool)
    [expr| false |] -> return (M.empty, TBool)
    LitI _          -> return (M.empty, TInt)
    _ -> throwError $ "cannot infer: " ++ show e0
  where
    -- both operands Int (the tutorial's Eql also compares Ints)
    binOp env' a b res = do
        (s1, t1) <- infer env' a
        (s2, t2) <- infer (applyEnv s1 env') b
        s3 <- unify (applyT s2 t1) TInt
        s4 <- unify (applyT s3 t2) TInt
        return (foldr1 composeSubst [s4, s3, s2, s1], res)

inferScheme :: TypeEnv -> Expr -> Either String Scheme
inferScheme env e =
    case evalState (runExceptT (infer env e)) 0 of
        Left err     -> Left err
        Right (s, t) -> Right $ generalize (applyEnv s env) (applyT s t)

-- --------------------------------------------------------------------
-- Call-by-value evaluation; 'fix' ties a lazy knot through the
-- closure's environment
-- --------------------------------------------------------------------

type Env = M.Map Id Value

data Value = VInt Int
           | VBool Bool
           | VClosure Id Expr Env

instance Show Value where
    show (VInt n)         = show n
    show (VBool True)     = "true"
    show (VBool False)    = "false"
    show (VClosure x e _) = "<<closure>> \\" ++ unId x ++ " -> " ++ ppr 0 e

eval :: Env -> Expr -> Value
eval env e0 = case e0 of
    [expr| \ $p1 -> $e1 |] -> case p1 of
        [q1] -> VClosure (paramId q1) e1 env
        _    -> error "internal error: lambda not desugared to one parameter"
    [expr| let $x1 = $e1 in $e2 |] ->
        let v = eval env e1 in v `seq` eval (M.insert x1 v env) e2
    [expr| if $e1 then $e2 else $e3 |] ->
        case eval env e1 of
            VBool True  -> eval env e2
            VBool False -> eval env e3
            v           -> error $ "if condition is not a boolean: " ++ show v
    [expr| fix $e1 |] ->
        case eval env e1 of
            VClosure x body cenv ->
                let v = eval (M.insert x v cenv) body in v
            v -> error $ "cannot fix non-function: " ++ show v
    [expr| $e1 == $e2 |] ->
        case (eval env e1, eval env e2) of
            (VInt a,  VInt b)  -> VBool (a == b)
            (VBool a, VBool b) -> VBool (a == b)
            (v1, v2) -> error $ "cannot compare " ++ show v1 ++ " == " ++ show v2
    [expr| $e1 + $e2 |] -> vArith "+" (+) (eval env e1) (eval env e2)
    [expr| $e1 - $e2 |] -> vArith "-" (-) (eval env e1) (eval env e2)
    [expr| $e1 * $e2 |] -> vArith "*" (*) (eval env e1) (eval env e2)
    [expr| $e1 $e2 |] ->
        case eval env e1 of
            VClosure x body cenv ->
                let arg = eval env e2
                in arg `seq` eval (M.insert x arg cenv) body
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
-- Pretty-printer for expressions (used by closure printing and tests)
-- --------------------------------------------------------------------

ppr :: Int -> Expr -> String
ppr d e0 = case e0 of
    [expr| \ $p1 -> $e1 |] ->
        paren (d > 0) $ "\\" ++ unwords (map (unId . paramId) p1)
                          ++ " -> " ++ ppr 0 e1
    [expr| let rec $x1 = $e1 in $e2 |] ->
        paren (d > 0) $ "let rec " ++ unId x1 ++ " = " ++ ppr 0 e1
                          ++ " in " ++ ppr 0 e2
    [expr| let $x1 = $e1 in $e2 |] ->
        paren (d > 0) $ "let " ++ unId x1 ++ " = " ++ ppr 0 e1
                          ++ " in " ++ ppr 0 e2
    [expr| if $e1 then $e2 else $e3 |] ->
        paren (d > 0) $ "if " ++ ppr 0 e1 ++ " then " ++ ppr 0 e2
                          ++ " else " ++ ppr 0 e3
    [expr| fix $e1 |]    -> paren (d > 0) $ "fix " ++ ppr 5 e1
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
-- Programs: infer then evaluate each declaration in order; bare
-- expression declarations produce "value : type" output lines
-- --------------------------------------------------------------------

type Session = (TypeEnv, Env)

emptySession :: Session
emptySession = (M.empty, M.empty)

execDecl :: Session -> Decl -> Either String (Session, Maybe String)
execDecl (tenv, env) d = case desugarDecl d of
    [decl| let $x1 = $e1 ; |] -> do
        sc <- inferScheme tenv e1
        let v = eval env e1
        return ((M.insert x1 sc tenv, M.insert x1 v env), Nothing)
    [decl| $e1 ; |] -> do
        sc <- inferScheme tenv e1
        return ((tenv, env), Just (show (eval env e1) ++ " : " ++ pprScheme sc))
    other -> Left $ "internal error: undesugared declaration " ++ show other

runProgram :: String -> Either String [String]
runProgram src = go emptySession (parseProgram src) []
  where
    go _ [] acc = Right (reverse acc)
    go sess (d : ds) acc = do
        (sess', out) <- execDecl sess d
        go sess' ds (maybe acc (: acc) out)

-- --------------------------------------------------------------------
-- REPL: a session that accumulates declarations, like the tutorial's
-- ': let x = ...' / expression interaction
-- --------------------------------------------------------------------

repl :: IO ()
repl = do
    putStrLn "poly - Hindley-Milner inference on RTK (:q to quit)"
    loop emptySession
  where
    loop sess = do
        putStr "poly> "
        hFlush stdout
        end <- isEOF
        if end then putStrLn "" else do
            line <- getLine
            case words line of
                []     -> loop sess
                [":q"] -> return ()
                _      -> do
                    let src = if last (concat (words line)) == ';'
                                then line else line ++ " ;"
                    result <- try (run sess src)
                    case result of
                        Left e -> do
                            putStrLn $ takeWhile (/= '\n') $
                                show (e :: SomeException)
                            loop sess
                        Right sess' -> loop sess'
    run sess src = do
        sess' <- foldM step sess (parseProgram src)
        _ <- evaluate sess'
        return sess'
    step sess d = case execDecl sess d of
        Left err           -> putStrLn ("type error: " ++ err) >> return sess
        Right (sess', out) -> do
            maybe (return ()) putStrLn out
            _ <- evaluate (fst sess')
            return sess'

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
        typeOf  = fmap pprScheme . inferScheme M.empty . desugarE . parseExpr

    putStrLn "== list quasi-quotation =="
    check "params list metavar binds all parameters"
        (case parseExpr "\\x y z -> x" of
            [expr| \ $p1 -> $e1 |] -> map (unId . paramId) p1
            _                      -> [])
        ["x", "y", "z"]
    check "program metavar binds the declaration list"
        (length (parseProgram "let a = 1; let b = 2; a + b;"))
        3

    putStrLn "== desugaring as QQ rewrites =="
    check "multi-parameter lambdas curry"
        (desugarE (parseExpr "\\x y -> x"))
        (desugarE (parseExpr "\\x -> \\y -> x"))
    check "let rec in becomes fix"
        (desugarE (parseExpr "let rec go = \\n -> go n in go"))
        (desugarE (parseExpr "let go = fix (\\go -> \\n -> go n) in go"))
    check "function declarations become lambdas"
        (desugarDecl (head (parseProgram "let f x y = x + y;")))
        (desugarDecl (head (parseProgram "let f = \\x -> \\y -> x + y;")))

    putStrLn "== chapter 7: inference =="
    check "identity" (typeOf "\\x -> x") (Right "a -> a")
    check "const"    (typeOf "\\x y -> x") (Right "a -> b -> a")
    check "compose"
        (typeOf "\\f g x -> f (g x)")
        (Right "(a -> b) -> (c -> a) -> c -> b")
    check "application instantiates"
        (typeOf "(\\x -> x) 5") (Right "Int")
    check "let-polymorphism (id at two types)"
        (typeOf "let id0 = \\x -> x in if id0 true then id0 1 else id0 2")
        (Right "Int")
    check "lambda-bound variables stay monomorphic"
        (typeOf "\\f -> if f true then f 1 else f 0")
        (Left "cannot unify Bool with Int")
    check "fix"
        (typeOf "fix (\\go -> \\n -> if n == 0 then 0 else go (n - 1))")
        (Right "Int -> Int")
    check "let rec in"
        (typeOf "let rec count = \\n -> if n == 0 then 0 else count (n - 1) \
                \in count")
        (Right "Int -> Int")
    check "occurs check"
        (typeOf "\\x -> x x")
        (Left "infinite type: t0 = t0 -> t1")
    check "condition must be Bool"
        (typeOf "if 1 then 2 else 3")
        (Left "cannot unify Int with Bool")
    check "branches must agree"
        (typeOf "if true then 1 else false")
        (Left "cannot unify Int with Bool")
    check "operands must be Int"
        (typeOf "1 + true")
        (Left "cannot unify Bool with Int")
    check "unbound variable"
        (typeOf "x + 1")
        (Left "unbound variable: x")

    putStrLn "== programs: infer + evaluate =="
    check "fibonacci via let rec"
        (runProgram "let rec fib n = if n == 0 then 0 else \
                    \if n == 1 then 1 else fib (n - 1) + fib (n - 2); \
                    \fib 10;")
        (Right ["55 : Int"])
    check "factorial via explicit fix"
        (runProgram "let fact = fix (\\f n -> if n == 0 then 1 \
                    \else n * f (n - 1)); fact 5;")
        (Right ["120 : Int"])
    check "polymorphic declaration reused at two types"
        (runProgram "let id0 = \\x -> x; if id0 true then id0 1 else id0 2;")
        (Right ["1 : Int"])
    check "declarations see earlier bindings statically"
        (runProgram "let n = 1; let f = \\m -> n + m; let n = 100; f 1;")
        (Right ["2 : Int"])
    check "higher-order program"
        (runProgram "let twice f x = f (f x); let inc n = n + 1; \
                    \twice inc 0; twice (twice inc) 0;")
        (Right ["2 : Int", "4 : Int"])
    check "a type error aborts the program"
        (runProgram "let x = 1 + true; x;")
        (Left "cannot unify Bool with Int")
    check "inferred declaration scheme is printed for expressions"
        (runProgram "let id0 = \\x -> x; id0;")
        (Right ["<<closure>> \\x -> x : a -> a"])

    n <- readIORef failures
    if n == 0
        then putStrLn "\nAll poly tests passed."
        else do
            putStrLn $ "\n" ++ show n ++ " poly test(s) FAILED."
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["repl"] -> repl
        _        -> runTests
