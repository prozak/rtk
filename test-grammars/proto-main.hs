{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE PatternSynonyms #-}

-- "Write You a Haskell" on RTK, chapters 8-12 (ProtoHaskell-lite):
-- Poly extended with algebraic data types, case expressions with nested
-- patterns, and a renamer. Frontend generated from proto.pg; blocks use
-- explicit { ; } delimiters (layout-sensitive lexing for RTK is tracked
-- in github issue #95).
--
-- Pipeline per declaration, mirroring the tutorial's compiler passes:
--
--   parse    - generated parser
--   data     - (ch 10) 'data' declarations become constructor schemes:
--              data List a = Nil | Cons a (List a)
--              gives Cons :: forall a. a -> List a -> List a
--   rename   - (ch 11) scope checking before inference: unknown
--              variables/constructors, constructor arity in patterns,
--              duplicate pattern variables, unknown type variables
--   desugar  - (ch 12, lite) QQ rewrites as in poly: currying, let rec
--              to fix; nested patterns are matched directly by the
--              evaluator rather than compiled to simple case trees
--   infer    - algorithm W with parameterized TData types and case/
--              pattern inference
--   eval     - call-by-value; constructors are curried values
--
-- Run modes:
--   proto-main         run the test suite (make test-proto)
--   proto-main repl    stateful session REPL (make repl-proto)

import Control.Exception (SomeException, evaluate, try)
-- mtl >= 2.3 no longer re-exports Control.Monad from the transformer modules
import Control.Monad (foldM, when)
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

import ProtoLexer
import ProtoParser
import ProtoQQ

-- Wrapper and token constructors (everything structural goes through QQ).
-- Explicitly bidirectional: matching ignores the constructors' RtkPos
-- field, construction supplies rtkNoPos.
pattern LitI :: Int -> Expr
pattern LitI n <- Ctr__Expr__0 _ n
  where LitI n = Ctr__Expr__0 rtkNoPos n

pattern IdN :: String -> Id
pattern IdN s <- Ctr__Id__0 _ s
  where IdN s = Ctr__Id__0 rtkNoPos s

pattern ConIdN :: String -> ConId
pattern ConIdN s <- Ctr__ConId__0 _ s
  where ConIdN s = Ctr__ConId__0 rtkNoPos s

pattern ParamN :: Id -> Param
pattern ParamN x <- Ctr__Param__0 _ x
  where ParamN x = Ctr__Param__0 rtkNoPos x

pattern PArgN :: Pat -> PArg
pattern PArgN q <- Ctr__PArg__0 _ q
  where PArgN q = Ctr__PArg__0 rtkNoPos q

pattern TyVarN :: Id -> TyVar
pattern TyVarN x <- Ctr__TyVar__0 _ x
  where TyVarN x = Ctr__TyVar__0 rtkNoPos x

pattern FieldN :: Ty -> Field
pattern FieldN t <- Ctr__Field__0 _ t
  where FieldN t = Ctr__Field__0 rtkNoPos t

-- Pattern AST (the pattern-matching code reads better with names)
pattern PLit :: Int -> Pat
pattern PLit n <- Ctr__Pat__0 _ n
  where PLit n = Ctr__Pat__0 rtkNoPos n
pattern PWild :: Pat
pattern PWild <- Ctr__Pat__1 _
  where PWild = Ctr__Pat__1 rtkNoPos
pattern PTrue :: Pat
pattern PTrue <- Ctr__Pat__2 _
  where PTrue = Ctr__Pat__2 rtkNoPos
pattern PFalse :: Pat
pattern PFalse <- Ctr__Pat__3 _
  where PFalse = Ctr__Pat__3 rtkNoPos
pattern PVar :: Id -> Pat
pattern PVar x <- Ctr__Pat__4 _ x
  where PVar x = Ctr__Pat__4 rtkNoPos x
pattern PCon0 :: ConId -> Pat
pattern PCon0 c <- Ctr__Pat__5 _ c
  where PCon0 c = Ctr__Pat__5 rtkNoPos c
pattern PConP :: ConId -> [PArg] -> Pat
pattern PConP c args <- Ctr__Pat__7 _ c args
  where PConP c args = Ctr__Pat__7 rtkNoPos c args

-- Surface type AST from data declarations
pattern TyCon :: ConId -> Ty
pattern TyCon c <- Ctr__Ty__0 _ c
  where TyCon c = Ctr__Ty__0 rtkNoPos c
pattern TyVarRef :: Id -> Ty
pattern TyVarRef x <- Ctr__Ty__1 _ x
  where TyVarRef x = Ctr__Ty__1 rtkNoPos x
pattern TyApply :: Ty -> Ty -> Ty
pattern TyApply t1 t2 <- Ctr__Ty__3 _ t1 t2
  where TyApply t1 t2 = Ctr__Ty__3 rtkNoPos t1 t2
pattern TyArrow :: Ty -> Ty -> Ty
pattern TyArrow t1 t2 <- Ctr__Ty__5 _ t1 t2
  where TyArrow t1 t2 = Ctr__Ty__5 rtkNoPos t1 t2

unId :: Id -> String
unId (IdN s)     = s
unId (Anti_Id s) = '$' : s
unId other       = show other

unConId :: ConId -> String
unConId (ConIdN s)     = s
unConId (Anti_ConId s) = '$' : s
unConId other          = show other

paramId :: Param -> Id
paramId (ParamN x) = x
paramId other      = error $ "unexpected param: " ++ show other

unProto :: Proto -> [Decl]
unProto [proto| $d |] = d
unProto other         = error $ "unexpected start wrapper: " ++ show other

-- Prelude.id: ProtoQQ also exports an 'id' quasi-quoter for the Id type
parseProgram :: String -> [Decl]
parseProgram src =
    unProto $ either errorWithoutStackTrace Prelude.id (scanTokens src >>= parseProto)

parseExpr :: String -> Expr
parseExpr src = case parseProgram (src ++ " ;") of
    [ [decl| $e1 ; |] ] -> e1
    _                   -> error $ "not a single expression: " ++ src

-- --------------------------------------------------------------------
-- Types, schemes, substitutions (Poly's, plus parameterized TData)
-- --------------------------------------------------------------------

data Type = TVar String
          | TData String [Type]
          | TArr Type Type
          deriving (Eq, Ord, Show)

tInt, tBool :: Type
tInt  = TData "Int" []
tBool = TData "Bool" []

data Scheme = Forall [String] Type
              deriving (Eq, Show)

pprType :: Int -> Type -> String
pprType d t0 = case t0 of
    TArr a b     -> paren (d > 0) $ pprType 1 a ++ " -> " ++ pprType 0 b
    TData n []   -> n
    TData n args -> paren (d > 4) $ unwords (n : map (pprType 5) args)
    TVar v       -> v
  where paren True  s = "(" ++ s ++ ")"
        paren False s = s

pprScheme :: Scheme -> String
pprScheme (Forall _ t) = pprType 0 (rename t)
  where
    names   = map (:[]) ['a' .. 'z'] ++ [ 'a' : show k | k <- [1 :: Int ..] ]
    table   = M.fromList (zip (nub (collect t)) names)
    collect (TVar v)       = [v]
    collect (TArr a b)     = collect a ++ collect b
    collect (TData _ args) = concatMap collect args
    rename (TVar v)        = TVar (M.findWithDefault v v table)
    rename (TArr a b)      = TArr (rename a) (rename b)
    rename (TData n args)  = TData n (map rename args)

type Subst   = M.Map String Type
type TypeEnv = M.Map Id Scheme
type Infer a = ExceptT String (State Int) a

fresh :: Infer Type
fresh = do
    n <- get
    put (n + 1)
    return $ TVar ("t" ++ show n)

applyT :: Subst -> Type -> Type
applyT s t@(TVar v)     = M.findWithDefault t v s
applyT s (TArr t1 t2)   = TArr (applyT s t1) (applyT s t2)
applyT s (TData n args) = TData n (map (applyT s) args)

ftvT :: Type -> S.Set String
ftvT (TVar v)       = S.singleton v
ftvT (TArr t1 t2)   = ftvT t1 `S.union` ftvT t2
ftvT (TData _ args) = S.unions (map ftvT args)

applyScheme :: Subst -> Scheme -> Scheme
applyScheme s (Forall vs t) = Forall vs (applyT (foldr M.delete s vs) t)

ftvScheme :: Scheme -> S.Set String
ftvScheme (Forall vs t) = ftvT t `S.difference` S.fromList vs

applyEnv :: Subst -> TypeEnv -> TypeEnv
applyEnv s = M.map (applyScheme s)

ftvEnv :: TypeEnv -> S.Set String
ftvEnv = S.unions . map ftvScheme . M.elems

composeSubst :: Subst -> Subst -> Subst
composeSubst s1 s2 = M.map (applyT s1) s2 `M.union` s1

unify :: Type -> Type -> Infer Subst
unify (TArr a1 b1) (TArr a2 b2) = do
    s1 <- unify a1 a2
    s2 <- unify (applyT s1 b1) (applyT s1 b2)
    return (s2 `composeSubst` s1)
unify (TData n1 as1) (TData n2 as2)
    | n1 == n2 && length as1 == length as2 =
        foldM step M.empty (zip as1 as2)
  where step s (a1, a2) = do
            s' <- unify (applyT s a1) (applyT s a2)
            return (s' `composeSubst` s)
unify (TVar v) t = bindVar v t
unify t (TVar v) = bindVar v t
unify t1 t2 = throwError $
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

-- --------------------------------------------------------------------
-- Chapter 10: data declarations become constructor schemes
-- --------------------------------------------------------------------

data ConInfo = ConInfo
    { conData   :: String
    , conArity  :: Int
    , conScheme :: Scheme
    } deriving Show

type DataEnv = M.Map String Int      -- type constructor name -> arity
type ConEnv  = M.Map String ConInfo  -- value constructor name -> info

builtinData :: DataEnv
builtinData = M.fromList [("Int", 0), ("Bool", 0)]

-- Surface Ty AST -> Type, under the data declaration's type variables.
tyToType :: DataEnv -> S.Set String -> Ty -> Either String Type
tyToType denv tvs = go
  where
    go (TyArrow a b) = TArr <$> go a <*> go b
    go (TyVarRef x)
        | unId x `S.member` tvs = Right (TVar (unId x))
        | otherwise = Left $ "unknown type variable: " ++ unId x
    go t = do
        (name, args) <- spine t []
        case M.lookup name denv of
            Nothing -> Left $ "unknown type constructor: " ++ name
            Just k
                | k == length args -> TData name <$> mapM go args
                | otherwise -> Left $
                    "type constructor " ++ name ++ " expects " ++ show k
                    ++ " argument(s), got " ++ show (length args)
    spine (TyCon c) acc       = Right (unConId c, acc)
    spine (TyApply t1 t2) acc = spine t1 (t2 : acc)
    spine t _ = Left $ "malformed type: " ++ show t

processData :: (DataEnv, ConEnv) -> ConId -> [TyVar] -> [ConDef]
            -> Either String (DataEnv, ConEnv)
processData (denv, cenv) c tyvars condefs = do
    let name = unConId c
        vars = [ unId x | TyVarN x <- tyvars ]
    when (name `M.member` denv) $
        Left $ "duplicate data type: " ++ name
    when (length (nub vars) /= length vars) $
        Left $ "duplicate type variable in data " ++ name
    let denv'  = M.insert name (length vars) denv
        result = TData name (map TVar vars)
    cenv' <- foldM (addCon denv' (S.fromList vars) vars result) cenv condefs
    return (denv', cenv')
  where
    addCon denv' tvs vars result acc condef = case condef of
        Ctr__ConDef__0 _ cc fields -> do
            let cname = unConId cc
            when (cname `M.member` acc) $
                Left $ "duplicate constructor: " ++ cname
            fieldTys <- mapM (\(FieldN t) -> tyToType denv' tvs t) fields
            let ty = foldr TArr result fieldTys
            return $ M.insert cname
                (ConInfo (unConId c) (length fieldTys) (Forall vars ty)) acc
        other -> Left $ "malformed constructor definition: " ++ show other

-- --------------------------------------------------------------------
-- Chapter 11: the renamer - scope and arity checking before inference
-- --------------------------------------------------------------------

-- Variables bound by one pattern, rejecting duplicates and checking
-- constructor existence and arity.
patVars :: ConEnv -> Pat -> Either String [Id]
patVars cenv p0 = do
    vs <- go p0
    let names = map unId vs
        dups  = [ n | n <- nub names, length (filter (== n) names) > 1 ]
    case dups of
        (n : _) -> Left $ "duplicate variable in pattern: " ++ n
        []      -> Right vs
  where
    go (PVar x)  = Right [x]
    go PWild     = Right []
    go (PLit _)  = Right []
    go PTrue     = Right []
    go PFalse    = Right []
    go (PCon0 c) = [] <$ conOfArity c 0
    go (PConP c args) = do
        _ <- conOfArity c (length args)
        concat <$> mapM (\(PArgN q) -> go q) args
    go other = Left $ "malformed pattern: " ++ show other
    conOfArity c k = case M.lookup (unConId c) cenv of
        Nothing -> Left $ "unknown constructor in pattern: " ++ unConId c
        Just ci
            | conArity ci == k -> Right ci
            | otherwise -> Left $
                "constructor " ++ unConId c ++ " expects "
                ++ show (conArity ci) ++ " argument(s) in pattern, got "
                ++ show k

checkExpr :: ConEnv -> S.Set Id -> Expr -> Either String ()
checkExpr cenv = go
  where
    go bound e0 = case e0 of
        [expr| \ $p1 -> $e1 |] ->
            go (foldr (S.insert . paramId) bound p1) e1
        [expr| let rec $x1 = $e1 in $e2 |] -> do
            go (S.insert x1 bound) e1
            go (S.insert x1 bound) e2
        [expr| let $x1 = $e1 in $e2 |] -> do
            go bound e1
            go (S.insert x1 bound) e2
        [expr| case $e1 of { $a1 } |] -> do
            go bound e1
            mapM_ (checkAlt bound) a1
        [expr| $x1 |]
            | x1 `S.member` bound -> Right ()
            | otherwise -> Left $ "unbound variable: " ++ unId x1
        [expr| $c1 |]
            | unConId c1 `M.member` cenv -> Right ()
            | otherwise -> Left $ "unknown constructor: " ++ unConId c1
        [expr| $e1 $e2 |]                  -> go bound e1 >> go bound e2
        [expr| $e1 == $e2 |]               -> go bound e1 >> go bound e2
        [expr| $e1 + $e2 |]                -> go bound e1 >> go bound e2
        [expr| $e1 - $e2 |]                -> go bound e1 >> go bound e2
        [expr| $e1 * $e2 |]                -> go bound e1 >> go bound e2
        [expr| if $e1 then $e2 else $e3 |] -> mapM_ (go bound) [e1, e2, e3]
        [expr| fix $e1 |]                  -> go bound e1
        _                                  -> Right ()
    checkAlt bound [alt| $q1 -> $e1 |] = do
        vs <- patVars cenv q1
        go (foldr S.insert bound vs) e1
    checkAlt _ other = Left $ "malformed alternative: " ++ show other

-- --------------------------------------------------------------------
-- Chapter 12 (lite): desugaring as QQ rewrites, as in poly; nested
-- patterns are handled directly by the evaluator's matcher
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
    [expr| case $e1 of { $a1 } |] ->
        let e2 = desugarE e1
            a2 = map desugarAlt a1
        in [expr| case $e2 of { $a2 } |]
    _ -> gmapT (mkT desugarE) e0
  where
    desugarAlt [alt| $q1 -> $e1 |] = let e2 = desugarE e1 in [alt| $q1 -> $e2 |]
    desugarAlt other               = other

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
-- Inference: algorithm W plus constructors and case
-- --------------------------------------------------------------------

infer :: ConEnv -> TypeEnv -> Expr -> Infer (Subst, Type)
infer cenv env e0 = case e0 of
    [expr| \ $p1 -> $e1 |] -> case p1 of
        [q1] -> do
            tv <- fresh
            (s1, t1) <- infer cenv (M.insert (paramId q1) (Forall [] tv) env) e1
            return (s1, TArr (applyT s1 tv) t1)
        _ -> throwError "internal error: lambda not desugared to one parameter"
    [expr| let $x1 = $e1 in $e2 |] -> do
        (s1, t1) <- infer cenv env e1
        let env1 = applyEnv s1 env
            sc   = generalize env1 (applyT s1 t1)
        (s2, t2) <- infer cenv (M.insert x1 sc env1) e2
        return (s2 `composeSubst` s1, t2)
    [expr| if $e1 then $e2 else $e3 |] -> do
        (s1, t1) <- infer cenv env e1
        s2 <- unify t1 tBool
        let env2 = applyEnv (s2 `composeSubst` s1) env
        (s3, t3) <- infer cenv env2 e2
        (s4, t4) <- infer cenv (applyEnv s3 env2) e3
        s5 <- unify (applyT s4 t3) t4
        return (foldr1 composeSubst [s5, s4, s3, s2, s1], applyT s5 t4)
    [expr| fix $e1 |] -> do
        (s1, t1) <- infer cenv env e1
        tv <- fresh
        s2 <- unify t1 (TArr tv tv)
        return (s2 `composeSubst` s1, applyT s2 tv)
    [expr| case $e1 of { $a1 } |] -> do
        (s1, t1) <- infer cenv env e1
        tv <- fresh
        (s, _, tres) <- foldM inferAlt (s1, applyT s1 t1, tv) a1
        return (s, tres)
      where
        inferAlt (s, tscrut, tres) [alt| $q1 -> $e2 |] = do
            (binds, tpat) <- inferPat cenv q1
            sp <- unify tscrut tpat
            let env1 = applyEnv (sp `composeSubst` s) env
                env2 = foldr (\(x, t) acc ->
                          M.insert x (Forall [] (applyT sp t)) acc)
                       env1 binds
            (sb, tb) <- infer cenv env2 e2
            se <- unify (applyT sb (applyT sp tres)) tb
            let s' = foldr1 composeSubst [se, sb, sp, s]
            return (s', applyT s' tscrut, applyT s' tres)
        inferAlt _ other =
            throwError $ "malformed alternative: " ++ show other
    [expr| $e1 == $e2 |] -> binOp env e1 e2 tBool
    [expr| $e1 + $e2 |]  -> binOp env e1 e2 tInt
    [expr| $e1 - $e2 |]  -> binOp env e1 e2 tInt
    [expr| $e1 * $e2 |]  -> binOp env e1 e2 tInt
    [expr| $e1 $e2 |] -> do
        (s1, t1) <- infer cenv env e1
        (s2, t2) <- infer cenv (applyEnv s1 env) e2
        tv <- fresh
        s3 <- unify (applyT s2 t1) (TArr t2 tv)
        return (foldr1 composeSubst [s3, s2, s1], applyT s3 tv)
    [expr| $x1 |] -> case M.lookup x1 env of
        Nothing -> throwError $ "unbound variable: " ++ unId x1
        Just sc -> do
            t <- instantiate sc
            return (M.empty, t)
    [expr| $c1 |] -> case M.lookup (unConId c1) cenv of
        Nothing -> throwError $ "unknown constructor: " ++ unConId c1
        Just ci -> do
            t <- instantiate (conScheme ci)
            return (M.empty, t)
    [expr| true |]  -> return (M.empty, tBool)
    [expr| false |] -> return (M.empty, tBool)
    LitI _          -> return (M.empty, tInt)
    _ -> throwError $ "cannot infer: " ++ show e0
  where
    binOp env' a b res = do
        (s1, t1) <- infer cenv env' a
        (s2, t2) <- infer cenv (applyEnv s1 env') b
        s3 <- unify (applyT s2 t1) tInt
        s4 <- unify (applyT s3 t2) tInt
        return (foldr1 composeSubst [s4, s3, s2, s1], res)

-- Pattern type plus monomorphic bindings for its variables.
inferPat :: ConEnv -> Pat -> Infer ([(Id, Type)], Type)
inferPat cenv p0 = case p0 of
    PLit _   -> return ([], tInt)
    PTrue    -> return ([], tBool)
    PFalse   -> return ([], tBool)
    PWild    -> do tv <- fresh; return ([], tv)
    PVar x   -> do tv <- fresh; return ([(x, tv)], tv)
    PCon0 c  -> do
        t <- conType c
        return ([], t)
    PConP c args -> do
        t0 <- conType c
        go t0 args []
      where
        go t [] binds = return (binds, t)
        go (TArr field rest) (PArgN q : qs) binds = do
            (bs, tq) <- inferPat cenv q
            s <- unify field tq
            let rest'  = applyT s rest
                binds' = [ (x, applyT s t') | (x, t') <- binds ++ bs ]
            go rest' qs binds'
        go t qs _ = throwError $
            "constructor applied to too many patterns: " ++ pprType 0 t
            ++ " with " ++ show (length qs) ++ " left"
    other -> throwError $ "malformed pattern: " ++ show other
  where
    conType c = case M.lookup (unConId c) cenv of
        Nothing -> throwError $ "unknown constructor: " ++ unConId c
        Just ci -> instantiate (conScheme ci)

inferScheme :: ConEnv -> TypeEnv -> Expr -> Either String Scheme
inferScheme cenv env e =
    case evalState (runExceptT (infer cenv env e)) 0 of
        Left err     -> Left err
        Right (s, t) -> Right $ generalize (applyEnv s env) (applyT s t)

-- --------------------------------------------------------------------
-- Evaluation: constructors are curried values; case matches nested
-- patterns directly
-- --------------------------------------------------------------------

type Env = M.Map Id Value

data Value = VInt Int
           | VBool Bool
           | VCon String Int [Value]   -- name, arity, collected arguments
           | VClosure Id Expr Env

instance Show Value where
    show (VInt n)          = show n
    show (VBool True)      = "true"
    show (VBool False)     = "false"
    show (VCon n _ [])     = n
    show (VCon n _ args)   = unwords (n : map showArg args)
      where showArg v@(VCon _ _ (_ : _)) = "(" ++ show v ++ ")"
            showArg v                    = show v
    show (VClosure x e _)  = "<<closure>> \\" ++ unId x ++ " -> " ++ ppr 0 e

matchPat :: Pat -> Value -> Maybe [(Id, Value)]
matchPat p0 v = case (p0, v) of
    (PWild, _)             -> Just []
    (PVar x, _)            -> Just [(x, v)]
    (PLit n, VInt m)       -> if n == m then Just [] else Nothing
    (PTrue, VBool b)       -> if b then Just [] else Nothing
    (PFalse, VBool b)      -> if b then Nothing else Just []
    (PCon0 c, VCon n _ []) -> if unConId c == n then Just [] else Nothing
    (PConP c args, VCon n _ vs)
        | unConId c == n && length args == length vs ->
            concat <$> sequence [ matchPat q u | (PArgN q, u) <- zip args vs ]
    _                      -> Nothing

eval :: ConEnv -> Env -> Expr -> Value
eval cenv env e0 = case e0 of
    [expr| \ $p1 -> $e1 |] -> case p1 of
        [q1] -> VClosure (paramId q1) e1 env
        _    -> error "internal error: lambda not desugared to one parameter"
    [expr| let $x1 = $e1 in $e2 |] ->
        let v = eval cenv env e1
        in v `seq` eval cenv (M.insert x1 v env) e2
    [expr| if $e1 then $e2 else $e3 |] ->
        case eval cenv env e1 of
            VBool True  -> eval cenv env e2
            VBool False -> eval cenv env e3
            v           -> error $ "if condition is not a boolean: " ++ show v
    [expr| fix $e1 |] ->
        case eval cenv env e1 of
            VClosure x body fenv ->
                let v = eval cenv (M.insert x v fenv) body in v
            v -> error $ "cannot fix non-function: " ++ show v
    [expr| case $e1 of { $a1 } |] ->
        let v = eval cenv env e1 in v `seq` go a1 v
      where
        go [] v = error $ "non-exhaustive patterns in case on: " ++ show v
        go ([alt| $q1 -> $e2 |] : rest) v =
            case matchPat q1 v of
                Just binds ->
                    eval cenv (foldr (uncurry M.insert) env binds) e2
                Nothing -> go rest v
        go (other : _) _ = error $ "malformed alternative: " ++ show other
    [expr| $e1 == $e2 |] ->
        case (eval cenv env e1, eval cenv env e2) of
            (VInt a,  VInt b)  -> VBool (a == b)
            (VBool a, VBool b) -> VBool (a == b)
            (v1, v2) -> error $ "cannot compare " ++ show v1 ++ " == " ++ show v2
    [expr| $e1 + $e2 |] -> vArith "+" (+) (eval cenv env e1) (eval cenv env e2)
    [expr| $e1 - $e2 |] -> vArith "-" (-) (eval cenv env e1) (eval cenv env e2)
    [expr| $e1 * $e2 |] -> vArith "*" (*) (eval cenv env e1) (eval cenv env e2)
    [expr| $e1 $e2 |] ->
        let arg = eval cenv env e2
        in arg `seq` case eval cenv env e1 of
            VClosure x body fenv -> eval cenv (M.insert x arg fenv) body
            VCon n arity args
                | length args < arity -> VCon n arity (args ++ [arg])
            v -> error $ "cannot apply non-function: " ++ show v
    [expr| $x1 |] ->
        case M.lookup x1 env of
            Just v  -> v
            Nothing -> error $ "unbound variable: " ++ unId x1
    [expr| $c1 |] ->
        case M.lookup (unConId c1) cenv of
            Just ci -> VCon (unConId c1) (conArity ci) []
            Nothing -> error $ "unknown constructor: " ++ unConId c1
    [expr| true |]  -> VBool True
    [expr| false |] -> VBool False
    LitI n          -> VInt n
    _               -> error $ "cannot evaluate: " ++ show e0

vArith :: String -> (Int -> Int -> Int) -> Value -> Value -> Value
vArith _  op (VInt a) (VInt b) = VInt (a `op` b)
vArith nm _  v1       v2       =
    error $ "cannot compute " ++ show v1 ++ " " ++ nm ++ " " ++ show v2

-- Minimal expression printer (closure display only)
ppr :: Int -> Expr -> String
ppr d e0 = case e0 of
    [expr| \ $p1 -> $e1 |] ->
        paren (d > 0) $ "\\" ++ unwords (map (unId . paramId) p1)
                          ++ " -> " ++ ppr 0 e1
    [expr| case $e1 of { $a1 } |] ->
        paren (d > 0) $ "case " ++ ppr 0 e1 ++ " of { "
            ++ show (length a1) ++ " alts }"
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
    [expr| $c1 |]        -> unConId c1
    LitI n               -> show n
    other                -> show other
  where paren True  s = "(" ++ s ++ ")"
        paren False s = s

-- --------------------------------------------------------------------
-- Sessions, programs, REPL
-- --------------------------------------------------------------------

data Session = Session
    { sDataEnv :: DataEnv
    , sConEnv  :: ConEnv
    , sTypeEnv :: TypeEnv
    , sEnv     :: Env
    }

emptySession :: Session
emptySession = Session builtinData M.empty M.empty M.empty

execDecl :: Session -> Decl -> Either String (Session, Maybe String)
execDecl sess d0 = case d0 of
    [decl| data $c1 $tyVar1 = $cd1 ; |] -> do
        (denv, cenv) <- processData (sDataEnv sess, sConEnv sess) c1 tyVar1 cd1
        return (sess { sDataEnv = denv, sConEnv = cenv }, Nothing)
    _ -> case desugarDecl d0 of
        [decl| let $x1 = $e1 ; |] -> do
            checkExpr (sConEnv sess) (M.keysSet (sTypeEnv sess)) e1
            sc <- inferScheme (sConEnv sess) (sTypeEnv sess) e1
            let v = eval (sConEnv sess) (sEnv sess) e1
            return ( sess { sTypeEnv = M.insert x1 sc (sTypeEnv sess)
                          , sEnv = M.insert x1 v (sEnv sess) }
                   , Nothing )
        [decl| $e1 ; |] -> do
            checkExpr (sConEnv sess) (M.keysSet (sTypeEnv sess)) e1
            sc <- inferScheme (sConEnv sess) (sTypeEnv sess) e1
            return ( sess
                   , Just (show (eval (sConEnv sess) (sEnv sess) e1)
                           ++ " : " ++ pprScheme sc) )
        other -> Left $ "internal error: undesugared declaration " ++ show other

runProgram :: String -> Either String [String]
runProgram src = go emptySession (parseProgram src) []
  where
    go _ [] acc = Right (reverse acc)
    go sess (d : ds) acc = do
        (sess', out) <- execDecl sess d
        go sess' ds (maybe acc (: acc) out)

repl :: IO ()
repl = do
    putStrLn "proto - ProtoHaskell-lite on RTK (:q to quit)"
    loop emptySession
  where
    loop sess = do
        putStr "proto> "
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
        _ <- evaluate (M.size (sTypeEnv sess'))
        return sess'
    step sess d = case execDecl sess d of
        Left err           -> putStrLn ("error: " ++ err) >> return sess
        Right (sess', out) -> do
            maybe (return ()) putStrLn out
            _ <- evaluate (M.size (sTypeEnv sess'))
            return sess'

-- --------------------------------------------------------------------
-- Test suite
-- --------------------------------------------------------------------

listPrelude :: String
listPrelude =
    "data List a = Nil | Cons a (List a); \
    \data Maybe a = Nothing | Just a; \
    \data Pair a b = MkPair a b; "

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
        runP s = runProgram (listPrelude ++ s)
        runCatch s = do
            r <- try (evaluate (length (show (runP s)) `seq` runP s))
            return $ case r of
                Left err -> Left ("exception: " ++ takeWhile (/= '\n')
                                    (show (err :: SomeException)))
                Right v  -> v

    putStrLn "== quasi-quotation over the new families =="
    check "case pattern binds the alternative list"
        (case parseExpr "case l of { Nil -> 0 ; Cons y ys -> 1 }" of
            [expr| case $e1 of { $a1 } |] -> length a1
            _                             -> 0)
        2
    check "alternative and pattern destructuring"
        (case parseExpr "case l of { Cons y ys -> y }" of
            [expr| case $e1 of { $a1 } |] -> case a1 of
                [ [alt| $q1 -> $e2 |] ] -> case q1 of
                    PConP c args -> unConId c ++ "/" ++ show (length args)
                    _            -> "?"
                _ -> "?"
            _ -> "?")
        "Cons/2"

    putStrLn "== chapter 10: data declarations and constructors =="
    check "list length"
        (runP "let rec len l = case l of { Nil -> 0 ; Cons x xs -> 1 + len xs }; \
              \len (Cons 1 (Cons 2 (Cons 3 Nil)));")
        (Right ["3 : Int"])
    check "map and sum over a list"
        (runP "let rec map0 f l = case l of { Nil -> Nil ; \
              \Cons x xs -> Cons (f x) (map0 f xs) }; \
              \let rec sum0 l = case l of { Nil -> 0 ; Cons x xs -> x + sum0 xs }; \
              \sum0 (map0 (\\n -> n * n) (Cons 1 (Cons 2 (Cons 3 Nil))));")
        (Right ["14 : Int"])
    check "map's inferred type is polymorphic"
        (runP "let rec map0 f l = case l of { Nil -> Nil ; \
              \Cons x xs -> Cons (f x) (map0 f xs) }; map0;")
        (Right ["<<closure>> \\f -> \\l -> case l of { 2 alts } \
               \: (a -> b) -> List a -> List b"])
    check "constructors are curried values"
        (runP "let j = Just; j 5;")
        (Right ["Just 5 : Maybe Int"])
    check "constructor values print with parentheses"
        (runP "Cons 1 (Cons 2 Nil);")
        (Right ["Cons 1 (Cons 2 Nil) : List Int"])

    putStrLn "== chapter 12 (lite): nested patterns =="
    check "nested constructor patterns with wildcards"
        (runP "let from p = case p of { MkPair (Just x) _ -> x ; \
              \MkPair Nothing y -> y }; \
              \from (MkPair (Just 10) 0); from (MkPair Nothing 7);")
        (Right ["10 : Int", "7 : Int"])
    check "literal and variable patterns mix"
        (runP "let f n = case n of { 0 -> 100 ; m -> m }; f 0; f 42;")
        (Right ["100 : Int", "42 : Int"])
    nonExh <- runCatch "let f m = case m of { Just x -> x }; f Nothing;"
    check "non-exhaustive match fails at runtime"
        nonExh
        (Left "exception: non-exhaustive patterns in case on: Nothing")

    putStrLn "== chapter 11: the renamer rejects =="
    check "unknown constructor"
        (runP "Foo 1;") (Left "unknown constructor: Foo")
    check "unknown constructor in pattern"
        (runP "let f x = case x of { Foo -> 1 };")
        (Left "unknown constructor in pattern: Foo")
    check "constructor pattern arity"
        (runP "let f x = case x of { Just -> 1 };")
        (Left "constructor Just expects 1 argument(s) in pattern, got 0")
    check "duplicate pattern variables"
        (runP "let f p = case p of { MkPair x x -> x };")
        (Left "duplicate variable in pattern: x")
    check "unknown type variable in a field"
        (runProgram "data D a = C b;")
        (Left "unknown type variable: b")
    check "unknown type constructor in a field"
        (runProgram "data D = C Foo;")
        (Left "unknown type constructor: Foo")
    check "type constructor arity in fields"
        (runP "data D = C (Pair Int);")
        (Left "type constructor Pair expects 2 argument(s), got 1")
    check "duplicate constructor"
        (runP "data D = MkPair;")
        (Left "duplicate constructor: MkPair")
    check "duplicate data type"
        (runP "data List = L;")
        (Left "duplicate data type: List")
    check "unbound variable"
        (runP "y + 1;") (Left "unbound variable: y")

    putStrLn "== inference over ADTs =="
    check "case branches must agree"
        (runP "let f m = case m of { Nothing -> 1 ; Just x -> true };")
        (Left "cannot unify Int with Bool")
    check "scrutinee must match the patterns"
        (runP "case 1 of { Nothing -> 0 };")
        (Left "cannot unify Int with Maybe t1")
    check "pattern variables get the field type"
        (runP "let f m = case m of { Just x -> x + 1 ; Nothing -> 0 }; f;")
        (Right ["<<closure>> \\m -> case m of { 2 alts } : Maybe Int -> Int"])
    check "polymorphic constructors generalize"
        (runP "let pair x = MkPair x x; pair;")
        (Right ["<<closure>> \\x -> MkPair x x : a -> Pair a a"])

    n <- readIORef failures
    if n == 0
        then putStrLn "\nAll proto tests passed."
        else do
            putStrLn $ "\n" ++ show n ++ " proto test(s) FAILED."
            exitFailure

main :: IO ()
main = do
    args <- getArgs
    case args of
        ["repl"] -> repl
        _        -> runTests
