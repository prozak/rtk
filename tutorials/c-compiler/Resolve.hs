{-# LANGUAGE QuasiQuotes #-}

-- The semantic pass, grown block-scoped for stage 7. It still assigns every
-- local a stack-frame offset and rejects what the grammar cannot -- a
-- variable used out of scope, or declared twice in the SAME scope (shadowing
-- an outer declaration is legal) -- but scoping changes what it returns.
--
-- The blog interleaves scope resolution with code generation; this compiler
-- keeps the passes separate, and shadowing is exactly what breaks the naive
-- interface between them: with two live `a`s, a name no longer identifies a
-- stack slot. So resolve ALPHA-RENAMES: it walks the tree with a stack of
-- scopes, gives each declaration a unique name (`a#0`, `a#1`, ...; `#` cannot
-- occur in a C identifier), rewrites every use to the unique name of the
-- declaration it refers to, and returns the renamed tree together with the
-- offset map keyed by unique names. Codegen's lookup-by-name stays exactly as
-- naive as before -- in the renamed tree it is correct.
--
-- Offsets grow monotonically across the whole function (the blog's scheme):
-- sibling blocks do not reuse slots, the frame is simply big enough for every
-- declaration that ever lives.
--
-- Structure follows what the tree can contain: declarations occur only in
-- block-item lists and scopes open only at compound statements, so the walk
-- is a hand recursion over BlockItem/Statement (a handful of QQ-pattern
-- cases); an expression can contain USES but never declarations, so renaming
-- one is a whole-subtree generic transform -- `everywhereM` over the derived
-- Data instances, in the Either monad so an out-of-scope use fails the walk.
module Resolve (resolve, VarMap) where

import Control.Monad (msum)
import qualified Data.Map as M
import Data.Data (Data)
import Data.Generics (everywhereM, mkM)

import CParser
import CQQ

-- Each unique-named local maps to its byte offset from %rbp (negative:
-- below the frame base).
type VarMap = M.Map String Int

-- Scopes, innermost first: source name -> unique name.
type Scopes = [M.Map String String]

-- Slots handed out so far; slot n lives at offset -4(n+1).
data Ctx = Ctx { slots :: VarMap, unique :: Int }

resolve :: Program -> Either String (VarMap, Program)
resolve [program| int $name ( ) { $stmts } |] = do
  (ctx, _, stmts2) <- renameItems (Ctx M.empty 0) [M.empty] stmts
  return (slots ctx, [program| int $name ( ) { $stmts2 } |])
resolve other = Left $ "resolve: unsupported program: " ++ show other

-- A block-item list runs in ONE scope that its declarations extend as the
-- walk passes them -- `a = 3; int a = 0;` inside a block assigns to the
-- OUTER a (official stage-7 declare_late), which falls out of threading the
-- scope left to right.
renameItems :: Ctx -> Scopes -> [BlockItem] -> Either String (Ctx, Scopes, [BlockItem])
renameItems ctx scopes [] = Right (ctx, scopes, [])
renameItems ctx scopes (item : rest) = do
  (ctx2, scopes2, item2) <- renameItem ctx scopes item
  (ctx3, scopes3, rest2) <- renameItems ctx2 scopes2 rest
  return (ctx3, scopes3, item2 : rest2)

renameItem :: Ctx -> Scopes -> BlockItem -> Either String (Ctx, Scopes, BlockItem)
renameItem ctx scopes (Decl p (DeclInit dp ident e)) = do
  e2 <- renameUses scopes e     -- the initializer sees the scope BEFORE the
                                -- declaration: `int a = a;` is out-of-scope
  (ctx2, scopes2, ident2) <- declare ctx scopes ident
  return (ctx2, scopes2, Decl p (DeclInit dp ident2 e2))
renameItem ctx scopes (Decl p (Declare dp ident)) = do
  (ctx2, scopes2, ident2) <- declare ctx scopes ident
  return (ctx2, scopes2, Decl p (Declare dp ident2))
renameItem ctx scopes (Stmt p s) = do
  (ctx2, s2) <- renameStmt ctx scopes s
  return (ctx2, scopes, Stmt p s2)
renameItem _ _ other = Left $ "resolve: unexpected block item: " ++ show other

-- Statements never extend the current scope; a compound statement opens a
-- fresh one, which pops with the recursion (the returned Scopes of the inner
-- walk is dropped).
renameStmt :: Ctx -> Scopes -> Statement -> Either String (Ctx, Statement)
renameStmt ctx scopes [statement| { $stmts } |] = do
  (ctx2, _, stmts2) <- renameItems ctx (M.empty : scopes) stmts
  return (ctx2, [statement| { $stmts2 } |])
renameStmt ctx scopes [statement| if ( $e ) $s1 else $s2 |] = do
  e2 <- renameUses scopes e
  (ctx2, s3) <- renameStmt ctx scopes s1
  (ctx3, s4) <- renameStmt ctx2 scopes s2
  return (ctx3, [statement| if ( $e2 ) $s3 else $s4 |])
renameStmt ctx scopes [statement| if ( $e ) $s1 |] = do
  e2 <- renameUses scopes e
  (ctx2, s3) <- renameStmt ctx scopes s1
  return (ctx2, [statement| if ( $e2 ) $s3 |])
renameStmt ctx scopes [statement| return $e ; |] = do
  e2 <- renameUses scopes e
  return (ctx, [statement| return $e2 ; |])
renameStmt ctx scopes [statement| $e ; |] = do
  e2 <- renameUses scopes e
  return (ctx, [statement| $e2 ; |])
renameStmt _ _ other = Left $ "resolve: unsupported statement: " ++ show other

-- Add a declaration to the CURRENT scope: an error only if that same scope
-- already has the name (shadowing an outer scope is what stage 7 is for).
declare :: Ctx -> Scopes -> Ident -> Either String (Ctx, Scopes, Ident)
declare _ [] _ = Left "resolve: no scope open (impossible)"
declare ctx (scope : outer) (Name p v)
  | v `M.member` scope = Left $ "duplicate declaration of variable '" ++ v ++ "'"
  | otherwise = Right ( Ctx (M.insert u (-4 * (unique ctx + 1)) (slots ctx)) (unique ctx + 1)
                      , M.insert v u scope : outer
                      , Name p u )
  where u = v ++ "#" ++ show (unique ctx)
declare _ _ other = Left $ "resolve: unexpected identifier node: " ++ show other

-- Rewrite every use in an expression to its unique name. Expressions contain
-- no declarations, so this is scope-blind within the subtree: one monadic SYB
-- transform over every Name node, failing on the first unresolvable one.
renameUses :: Data a => Scopes -> a -> Either String a
renameUses scopes = everywhereM (mkM rename)
  where
    rename (Name p v) = case lookupScopes v scopes of
      Just u  -> Right (Name p u)
      Nothing -> Left $ "undeclared variable '" ++ v ++ "'"

-- innermost scope wins: the first hit in the stack
lookupScopes :: String -> Scopes -> Maybe String
lookupScopes v = msum . map (M.lookup v)
