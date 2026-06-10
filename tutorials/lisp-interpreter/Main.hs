{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- lis.hs -- Peter Norvig's lis.py, ported to Haskell with RTK.
--
-- This is the interpreter from "(How to Write a (Lisp) Interpreter (in
-- Python))" (https://norvig.com/lispy.html), with the hand-written
-- tokenizer/reader replaced by the lexer/parser RTK generates from
-- scheme.pg, and with the special-form dispatch and macro expansion
-- written as quasi-quotation patterns over the generated AST.
--
-- Map to lis.py:
--   tokenize/read_from_tokens/atom  ->  scheme.pg (generated SchemeLexer/SchemeParser)
--   eval's  if x[0] == 'quote' ...  ->  eval's [expr| (quote $x) |] clauses
--   Env class                       ->  Env (chain of IORef'd Maps)
--   standard_env()                  ->  standardEnv
--   schemestr/repl                  ->  schemestr/repl
--   (beyond lis.py)                 ->  expand: derived forms via QQ rewrite rules
--
-- Usage:  lis             -- REPL
--         lis FILE.scm    -- run a file (a sequence of forms)
--         lis --test      -- run the test suite (Norvig's test cases)

import qualified Data.Map as M
import Data.Char (isSpace)
import Data.IORef
import Control.Exception (SomeException, evaluate, try)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hFlush, isEOF, stdout)

import SchemeLexer (scanTokens)
import SchemeParser
import SchemeQQ (expr)

-- ===========================================================================
-- Readable names for the generated AST constructors
-- ===========================================================================
-- RTK names constructors positionally (Ctr__Expr__N, in the order the
-- alternatives appear in scheme.pg), and every constructor carries the
-- source position of its first symbol in a leading RtkPos field. The
-- pattern synonyms give the constructors the names the rest of this file
-- uses: matching ignores the position, construction uses rtkNoPos.
-- Ctr__Expr__0 (the quasi-quoter's internal entry-point wrapper) and
-- Anti_Expr (antiquotation) never appear in a parsed program.

pattern Num :: Integer -> Expr
pattern Num n <- Ctr__Expr__1 _ n
  where Num n = Ctr__Expr__1 rtkNoPos n

pattern Flt :: Double -> Expr
pattern Flt d <- Ctr__Expr__2 _ d
  where Flt d = Ctr__Expr__2 rtkNoPos d

pattern Str :: String -> Expr
pattern Str s <- Ctr__Expr__3 _ s
  where Str s = Ctr__Expr__3 rtkNoPos s

pattern TrueL :: Expr
pattern TrueL <- Ctr__Expr__4 _
  where TrueL = Ctr__Expr__4 rtkNoPos

pattern FalseL :: Expr
pattern FalseL <- Ctr__Expr__5 _
  where FalseL = Ctr__Expr__5 rtkNoPos

pattern Sym :: String -> Expr
pattern Sym s <- Ctr__Expr__6 _ s
  where Sym s = Ctr__Expr__6 rtkNoPos s

pattern List :: [Expr] -> Expr
pattern List xs <- Ctr__Expr__7 _ xs
  where List xs = Ctr__Expr__7 rtkNoPos xs

-- ===========================================================================
-- Values (what eval returns) and environments
-- ===========================================================================
-- lis.py reuses Python lists/numbers for both programs and values. In a
-- typed setting programs (Expr) and runtime values (Value) are separate
-- types; (quote ...) reflects an Expr into a Value with datumValue.

data Value = VNum Integer
           | VFlt Double
           | VStr String
           | VBool Bool
           | VSym String
           | VList [Value]
           | VClosure [String] Expr Env
           | VPrim String ([Value] -> IO Value)
           | VUnspecified                        -- what define/set!/print return

type Frame = IORef (M.Map String Value)

-- An environment is a frame plus an optional outer environment (lis.py's
-- "class Env(dict)" with an outer pointer).
data Env = Env Frame (Maybe Env)

newEnv :: [(String, Value)] -> Maybe Env -> IO Env
newEnv kvs outer = do
    r <- newIORef (M.fromList kvs)
    pure (Env r outer)

-- lis.py's Env.find: the innermost frame in which the name is bound.
findFrame :: Env -> String -> IO Frame
findFrame (Env frame outer) name = do
    m <- readIORef frame
    if M.member name m
      then pure frame
      else case outer of
             Just env -> findFrame env name
             Nothing  -> schemeError ("unbound symbol: " ++ name)

lookupVar :: Env -> String -> IO Value
lookupVar env name = do
    frame <- findFrame env name
    m <- readIORef frame
    maybe (schemeError ("unbound symbol: " ++ name)) pure (M.lookup name m)

defineVar :: Env -> String -> Value -> IO ()
defineVar (Env frame _) name v = modifyIORef' frame (M.insert name v)

schemeError :: String -> a
schemeError msg = errorWithoutStackTrace ("scheme error: " ++ msg)

-- ===========================================================================
-- Macro expansion (beyond lis.py -- the RTK showcase)
-- ===========================================================================
-- Derived forms are rewritten into core forms before evaluation. Each rule
-- is a quasi-quotation pattern on the left and a quasi-quotation template
-- on the right: RTK parses both sides with the real Scheme parser at
-- compile time, so a typo in either side is a compile error here, not a
-- runtime surprise in user programs.

expand :: Expr -> Expr
-- quoted data is left exactly as written
expand q@[expr| (quote $x) |] = q
-- (when c b)   => (if c b #f)        (unless is the mirror image)
expand [expr| (when $c $b) |]   = expand [expr| (if $c $b #f) |]
expand [expr| (unless $c $b) |] = expand [expr| (if $c #f $b) |]
-- (and a b)    => (if a b #f)
expand [expr| (and $a $b) |]    = expand [expr| (if $a $b #f) |]
-- (or a b) must return a's value when a is truthy, so bind it first.
-- (Real Scheme uses a hygienic temporary; or-tmp can be captured if user
-- code names a variable or-tmp -- see the tutorial.)
expand [expr| (or $a $b) |]     = expand [expr| ((lambda (or-tmp) (if or-tmp or-tmp $b)) $a) |]
-- single-binding let, entirely as a QQ rewrite:
-- (let ((v e)) b)  =>  ((lambda (v) b) e)
expand [expr| (let (($v $e)) $b) |] = expand [expr| ((lambda ($v) $b) $e) |]
-- general let (any number of bindings, multi-expression body): ordinary
-- list manipulation on the AST, using the pattern synonyms
expand (List (Sym "let" : List bindings : body))
  | Just pairs <- mapM bindingPair bindings =
      let (vars, exps) = unzip pairs
          lam = List [Sym "lambda", List (map Sym vars), beginWrap body]
      in expand (List (lam : exps))
-- everything else: expand subexpressions
expand (List xs) = List (map expand xs)
expand e = e

bindingPair :: Expr -> Maybe (String, Expr)
bindingPair (List [Sym v, e]) = Just (v, e)
bindingPair _                 = Nothing

beginWrap :: [Expr] -> Expr
beginWrap [e] = e
beginWrap es  = List (Sym "begin" : es)

-- ===========================================================================
-- eval -- lis.py's eval, with QQ patterns instead of the x[0] == ... chain
-- ===========================================================================

eval :: Env -> Expr -> IO Value

-- (quote exp): the datum itself, unevaluated
eval _   [expr| (quote $x) |] = pure (datumValue x)

-- (if test conseq alt)
eval env [expr| (if $c $t $e) |] = do
    v <- eval env c
    eval env (if truthy v then t else e)

-- (define var exp)
eval env [expr| (define $v $e) |] = case v of
    Sym name -> do
        val <- eval env e
        defineVar env name val
        pure VUnspecified
    other -> schemeError ("define: expected a symbol, got " ++ showExpr other)

-- (set! var exp): assign in the innermost frame where var is bound
eval env [expr| (set! $v $e) |] = case v of
    Sym name -> do
        val <- eval env e
        frame <- findFrame env name
        modifyIORef' frame (M.insert name val)
        pure VUnspecified
    other -> schemeError ("set!: expected a symbol, got " ++ showExpr other)

-- (lambda (params...) body)
eval env [expr| (lambda $p $b) |] = pure (VClosure (paramNames p) b env)

-- variable reference
eval env (Sym name) = lookupVar env name

-- procedure call: evaluate the operator and the operands, then apply
eval env (List (f : args)) = do
    fv <- eval env f
    vs <- mapM (eval env) args
    apply fv vs

eval _ (List []) = schemeError "cannot evaluate the empty list ()"

-- atoms (numbers, strings, booleans) evaluate to themselves
eval _ atom = pure (datumValue atom)

apply :: Value -> [Value] -> IO Value
apply (VPrim _ f) args = f args
apply (VClosure params body cloEnv) args
  | length params == length args = do
      env <- newEnv (zip params args) (Just cloEnv)
      eval env body
  | otherwise = schemeError ("expected " ++ show (length params) ++
                             " argument(s), got " ++ show (length args))
apply v _ = schemeError ("not a procedure: " ++ schemestr v)

-- Scheme truth: everything except #f is true. (lis.py inherits Python
-- truthiness, where 0 and () are false -- a quirk, not a feature.)
truthy :: Value -> Bool
truthy (VBool False) = False
truthy _             = True

-- Reflect a piece of program text into a runtime value (used by quote and
-- by self-evaluating atoms).
datumValue :: Expr -> Value
datumValue (Num n)    = VNum n
datumValue (Flt d)    = VFlt d
datumValue (Str s)    = VStr (unescapeString s)
datumValue TrueL      = VBool True
datumValue FalseL     = VBool False
datumValue (Sym s)    = VSym s
datumValue (List xs)  = VList (map datumValue xs)
datumValue other      = schemeError ("not a datum: " ++ show other)

paramNames :: Expr -> [String]
paramNames (List ps) = map name ps
  where name (Sym s) = s
        name other   = schemeError ("lambda: parameter is not a symbol: " ++ showExpr other)
paramNames other = schemeError ("lambda: expected a parameter list, got " ++ showExpr other)

-- The string token arrives with its quotes and escapes intact.
unescapeString :: String -> String
unescapeString s = go (init (drop 1 s))
  where go ('\\':'n':cs) = '\n' : go cs
        go ('\\':'t':cs) = '\t' : go cs
        go ('\\':c:cs)   = c : go cs
        go (c:cs)        = c : go cs
        go []            = []

-- Print a program fragment the way it was written (for error messages).
showExpr :: Expr -> String
showExpr (Num n)    = show n
showExpr (Flt d)    = show d
showExpr (Str s)    = s
showExpr TrueL      = "#t"
showExpr FalseL     = "#f"
showExpr (Sym s)    = s
showExpr (List xs)  = "(" ++ unwords (map showExpr xs) ++ ")"
showExpr other      = show other

-- ===========================================================================
-- The standard environment (lis.py's standard_env)
-- ===========================================================================

standardEnv :: IO Env
standardEnv = newEnv bindings Nothing
  where
    bindings =
      [ ("+",  arith "+" (+) (+))
      , ("-",  VPrim "-" subPrim)
      , ("*",  arith "*" (*) (*))
      , ("/",  arith "/" div (/))     -- integer division on integers
      , ("max", arith "max" max max)
      , ("min", arith "min" min min)
      , ("<",  cmpPrim "<"  (== LT))
      , (">",  cmpPrim ">"  (== GT))
      , ("<=", cmpPrim "<=" (/= GT))
      , (">=", cmpPrim ">=" (/= LT))
      , ("=",  cmpPrim "="  (== EQ))
      , ("abs", VPrim "abs" absPrim)
      , ("append", VPrim "append" appendPrim)
      , ("apply", VPrim "apply" applyPrim)
      , ("begin", VPrim "begin" beginPrim)
      , ("car", VPrim "car" carPrim)
      , ("cdr", VPrim "cdr" cdrPrim)
      , ("cons", VPrim "cons" consPrim)
      , ("eq?", VPrim "eq?" equalPrim)     -- no object identity for immutable
      , ("equal?", VPrim "equal?" equalPrim) -- values; eq? = equal? here
      , ("length", VPrim "length" lengthPrim)
      , ("list", VPrim "list" (pure . VList))
      , ("list?", typePrim "list?" isList)
      , ("map", VPrim "map" mapPrim)
      , ("not", VPrim "not" notPrim)
      , ("null?", typePrim "null?" isNull)
      , ("number?", typePrim "number?" isNumber)
      , ("print", VPrim "print" printPrim)
      , ("procedure?", typePrim "procedure?" isProcedure)
      , ("round", VPrim "round" roundPrim)
      , ("symbol?", typePrim "symbol?" isSymbol)
      , ("pi", VFlt pi)
      ]

-- Numbers: integers and floats mix, promoting to float (lis.py gets this
-- from Python for free).
data NumPair = Ints Integer Integer | Dbls Double Double

numPair :: String -> Value -> Value -> NumPair
numPair _ (VNum a) (VNum b) = Ints a b
numPair _ (VNum a) (VFlt b) = Dbls (fromIntegral a) b
numPair _ (VFlt a) (VNum b) = Dbls a (fromIntegral b)
numPair _ (VFlt a) (VFlt b) = Dbls a b
numPair who a b = schemeError (who ++ ": expected numbers, got " ++
                               schemestr a ++ " and " ++ schemestr b)

arith :: String -> (Integer -> Integer -> Integer) -> (Double -> Double -> Double) -> Value
arith name iop dop = VPrim name go
  where go []     = schemeError (name ++ ": expected at least one argument")
        go (v:vs) = foldl step (pure v) vs
        step mv b = do a <- mv
                       pure $ case numPair name a b of
                                Ints x y -> VNum (iop x y)
                                Dbls x y -> VFlt (dop x y)

subPrim :: [Value] -> IO Value
subPrim [VNum n] = pure (VNum (negate n))   -- unary minus
subPrim [VFlt d] = pure (VFlt (negate d))
subPrim vs       = let VPrim _ go = arith "-" (-) (-) in go vs

cmpPrim :: String -> (Ordering -> Bool) -> Value
cmpPrim name f = VPrim name go
  where go [a, b] = pure $ VBool $ case numPair name a b of
                                     Ints x y -> f (compare x y)
                                     Dbls x y -> f (compare x y)
        go _ = schemeError (name ++ ": expected two arguments")

absPrim :: [Value] -> IO Value
absPrim [VNum n] = pure (VNum (abs n))
absPrim [VFlt d] = pure (VFlt (abs d))
absPrim _        = schemeError "abs: expected one number"

roundPrim :: [Value] -> IO Value
roundPrim [VFlt d] = pure (VNum (round d))
roundPrim [VNum n] = pure (VNum n)
roundPrim _        = schemeError "round: expected one number"

carPrim, cdrPrim, consPrim, appendPrim, lengthPrim :: [Value] -> IO Value
carPrim [VList (x:_)]  = pure x
carPrim _              = schemeError "car: expected a non-empty list"
cdrPrim [VList (_:xs)] = pure (VList xs)
cdrPrim _              = schemeError "cdr: expected a non-empty list"
consPrim [x, VList xs] = pure (VList (x:xs))
consPrim _             = schemeError "cons: expected a value and a list"
appendPrim vs          = VList . concat <$> mapM asList vs
  where asList (VList xs) = pure xs
        asList v          = schemeError ("append: expected lists, got " ++ schemestr v)
lengthPrim [VList xs]  = pure (VNum (fromIntegral (length xs)))
lengthPrim _           = schemeError "length: expected a list"

applyPrim, mapPrim, beginPrim, notPrim, equalPrim, printPrim :: [Value] -> IO Value
applyPrim [f, VList args] = apply f args
applyPrim _ = schemeError "apply: expected a procedure and a list of arguments"
mapPrim [f, VList xs] = VList <$> mapM (\x -> apply f [x]) xs
mapPrim _ = schemeError "map: expected a procedure and a list"
beginPrim [] = schemeError "begin: expected at least one expression"
beginPrim vs = pure (last vs)
notPrim [v] = pure (VBool (not (truthy v)))
notPrim _   = schemeError "not: expected one argument"
equalPrim [a, b] = pure (VBool (valueEq a b))
equalPrim _      = schemeError "equal?: expected two arguments"
printPrim [v] = putStrLn (schemestr v) >> pure VUnspecified
printPrim _   = schemeError "print: expected one argument"

typePrim :: String -> (Value -> Bool) -> Value
typePrim name f = VPrim name go
  where go [v] = pure (VBool (f v))
        go _   = schemeError (name ++ ": expected one argument")

isList, isNull, isNumber, isProcedure, isSymbol :: Value -> Bool
isList (VList _) = True
isList _         = False
isNull (VList []) = True
isNull _          = False
isNumber (VNum _) = True
isNumber (VFlt _) = True
isNumber _        = False
isProcedure (VClosure _ _ _) = True
isProcedure (VPrim _ _)      = True
isProcedure _                = False
isSymbol (VSym _) = True
isSymbol _        = False

valueEq :: Value -> Value -> Bool
valueEq (VNum a)  (VNum b)  = a == b
valueEq (VFlt a)  (VFlt b)  = a == b
valueEq (VNum a)  (VFlt b)  = fromIntegral a == b
valueEq (VFlt a)  (VNum b)  = a == fromIntegral b
valueEq (VStr a)  (VStr b)  = a == b
valueEq (VBool a) (VBool b) = a == b
valueEq (VSym a)  (VSym b)  = a == b
valueEq (VList a) (VList b) = length a == length b && and (zipWith valueEq a b)
valueEq _ _ = False

-- ===========================================================================
-- Printing values (lis.py's schemestr) and the REPL
-- ===========================================================================

schemestr :: Value -> String
schemestr (VNum n)      = show n
schemestr (VFlt d)      = show d
schemestr (VStr s)      = show s
schemestr (VBool True)  = "#t"
schemestr (VBool False) = "#f"
schemestr (VSym s)      = s
schemestr (VList vs)    = "(" ++ unwords (map schemestr vs) ++ ")"
schemestr (VClosure params _ _) = "#<procedure (" ++ unwords params ++ ")>"
schemestr (VPrim name _)        = "#<primitive " ++ name ++ ">"
schemestr VUnspecified  = "#<unspecified>"

parseForm :: String -> Either String Expr
parseForm src = scanTokens src >>= parseScheme

-- The generated lexer and parser encode error positions as "LINE:COL:message"
-- (machine-splittable); render them back human-readably for the console
renderError :: String -> String
renderError err =
    case span (/= ':') err of
        (l, ':' : rest1) | [(line, "")] <- (reads l :: [(Int, String)]) ->
            case span (/= ':') rest1 of
                (c, ':' : msg) | [(col, "")] <- (reads c :: [(Int, String)]) ->
                    "line " ++ show line ++ ", column " ++ show col ++ ": " ++ msg
                _ -> err
        _ -> err

-- Parse, expand, evaluate, and render one form, capturing any scheme error.
runForm :: Env -> String -> IO (Either String (Value, String))
runForm env input =
    case parseForm input of
        Left err -> pure (Left ("parse error: " ++ renderError err))
        Right ast -> do
            result <- try $ do
                v <- eval env (expand ast)
                let s = schemestr v
                _ <- evaluate (length s)
                pure (v, s)
            pure $ case result of
                Left e  -> Left (show (e :: SomeException))
                Right r -> Right r

repl :: Env -> IO ()
repl env = do
    putStr "lis.hs> "
    hFlush stdout
    end <- isEOF
    if end
      then putStrLn ""
      else do
        line <- getLine
        if all isSpace line
          then repl env
          else do
            result <- runForm env line
            case result of
              Left err                 -> putStrLn err
              Right (VUnspecified, _)  -> pure ()
              Right (_, s)             -> putStrLn s
            repl env

-- A file is a sequence of forms; wrapping it in (begin ...) lets the
-- single-expression parser run all of them in order. (lis.py programs do
-- the same thing with an explicit begin.)
runFile :: FilePath -> IO ()
runFile path = do
    src <- readFile path
    env <- standardEnv
    result <- runForm env ("(begin " ++ src ++ "\n)")
    case result of
        Left err                -> putStrLn err >> exitFailure
        Right (VUnspecified, _) -> pure ()
        Right (_, s)            -> putStrLn s

-- ===========================================================================
-- Test suite: Norvig's lis.py test cases (plus the derived forms)
-- ===========================================================================

-- (input, Just expected-output) or (input, Nothing) when the form, like
-- define, produces no output.
tests :: [(String, Maybe String)]
tests =
    -- from lis.py / lispytest.py
    [ ("(+ 2 2)", Just "4")
    , ("(+ (* 2 100) (* 1 10))", Just "210")
    , ("(if (> 6 5) (+ 1 1) (+ 2 2))", Just "2")
    , ("(if (< 6 5) (+ 1 1) (+ 2 2))", Just "4")
    , ("(define x 3)", Nothing)
    , ("x", Just "3")
    , ("(+ x x)", Just "6")
    , ("(begin (define y 1) (set! y (+ y 1)) (+ y 1))", Just "3")
    , ("((lambda (x) (+ x x)) 5)", Just "10")
    , ("(define twice (lambda (x) (* 2 x)))", Nothing)
    , ("(twice 5)", Just "10")
    , ("(define compose (lambda (f g) (lambda (x) (f (g x)))))", Nothing)
    , ("((compose list twice) 5)", Just "(10)")
    , ("(define repeat (lambda (f) (compose f f)))", Nothing)
    , ("((repeat twice) 5)", Just "20")
    , ("((repeat (repeat twice)) 5)", Just "80")
    , ("(define fact (lambda (n) (if (<= n 1) 1 (* n (fact (- n 1))))))", Nothing)
    , ("(fact 3)", Just "6")
    , ("(fact 50)", Just "30414093201713378043612608166064768844377641568960512000000000000")
    , ("(define my-abs (lambda (n) ((if (> n 0) + -) 0 n)))", Nothing)
    , ("(list (my-abs -3) (my-abs 0) (my-abs 3))", Just "(3 0 3)")
    , ("(define fib (lambda (n) (if (< n 2) 1 (+ (fib (- n 1)) (fib (- n 2))))))", Nothing)
    , ("(define range (lambda (a b) (if (= a b) (quote ()) (cons a (range (+ a 1) b)))))", Nothing)
    , ("(range 0 10)", Just "(0 1 2 3 4 5 6 7 8 9)")
    , ("(map fib (range 0 10))", Just "(1 1 2 3 5 8 13 21 34 55)")
    -- quote and data
    , ("(quote (1 2 three))", Just "(1 2 three)")
    , ("(car (quote (a b c)))", Just "a")
    , ("(cdr (quote (a b c)))", Just "(b c)")
    , ("(cons 1 (quote (2 3)))", Just "(1 2 3)")
    , ("(append (quote (1 2)) (quote (3 4)))", Just "(1 2 3 4)")
    , ("(apply + (list 1 2 3))", Just "6")
    , ("(symbol? (car (quote (a))))", Just "#t")
    , ("(equal? (list 1 2) (quote (1 2)))", Just "#t")
    -- atoms and the lexer
    , ("-7", Just "-7")
    , ("3.25", Just "3.25")
    , ("\"hello world\"", Just "\"hello world\"")
    , ("(- 5)", Just "-5")
    -- derived forms, handled by expand's QQ rewrite rules
    , ("(let ((x 2) (y 3)) (* x y))", Just "6")
    , ("(let ((r 10)) (* pi (* r r)))", Just "314.1592653589793")
    , ("(when (> 3 2) (quote yes))", Just "yes")
    , ("(unless (> 3 2) (quote yes))", Just "#f")
    , ("(and (> 2 1) (> 3 2))", Just "#t")
    , ("(and (> 1 2) (> 3 2))", Just "#f")
    , ("(or (> 1 2) 7)", Just "7")
    , ("(or 5 7)", Just "5")
    , ("(define circle-area (lambda (r) (when (> r 0) (* pi (* r r)))))", Nothing)
    , ("(circle-area 10)", Just "314.1592653589793")
    ]

runTests :: IO ()
runTests = do
    env <- standardEnv
    results <- mapM (runOne env) tests
    putStrLn ""
    let failed = length (filter not results)
    if failed == 0
      then putStrLn ("All " ++ show (length results) ++ " tests passed.")
      else do
        putStrLn (show failed ++ " of " ++ show (length results) ++ " tests FAILED.")
        exitFailure
  where
    runOne env (input, expected) = do
        result <- runForm env input
        let actual = case result of
                       Left err                -> Left err
                       Right (VUnspecified, _) -> Right Nothing
                       Right (_, s)            -> Right (Just s)
            ok = actual == Right expected
        putStrLn $ (if ok then "ok   " else "FAIL ") ++ input ++
                   case actual of
                     Right (Just s) | ok -> "  => " ++ s
                     _ | ok              -> ""
                     _ -> "\n  expected: " ++ show expected ++ "\n  actual:   " ++ show actual
        pure ok

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["--test"] -> runTests
        [path]     -> runFile path
        []         -> do
            putStrLn "lis.hs -- Norvig's lis.py on RTK (Ctrl-D to exit)"
            env <- standardEnv
            repl env
        _ -> putStrLn "usage: lis [--test | FILE.scm]"
