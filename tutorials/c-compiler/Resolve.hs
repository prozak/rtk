{-# LANGUAGE QuasiQuotes #-}

-- The compiler's first semantic pass. It walks the function body in order,
-- assigning each declared local a stack-frame offset and rejecting programs
-- the grammar cannot: a variable used before (or without) a declaration, or
-- declared twice. (Assignment to a non-lvalue is already a syntax error,
-- because the grammar only allows a bare identifier on the left of `=`.)
--
-- Collecting the variables a statement *uses* is a whole-subtree query, so it
-- is one SYB call -- `listify` over the derived Data instances -- rather than a
-- hand-written recursion over the entire Exp cascade. This is the division of
-- labour the plan calls for: quasi-quoters for targeted construction and
-- matching, generic programming for "find every X anywhere in this tree".
module Resolve (resolve, VarMap) where

import qualified Data.Map as M
import Data.Data (Data)
import Data.Generics (listify)

import CParser
import CQQ

-- Each local maps to its byte offset from %rbp (negative: below the frame base).
type VarMap = M.Map String Int

resolve :: Program -> Either String VarMap
resolve [program| int $name ( ) { $stmts } |] = resolveStmts stmts
resolve other = Left $ "resolve: unsupported program: " ++ show other

resolveStmts :: [Statement] -> Either String VarMap
resolveStmts = go M.empty
  where
    go env [] = Right env
    go env (s : rest) = case s of
      DeclInit _ ident e -> declare env rest (identName ident) (checkUses env e)
      Declare _ ident    -> declare env rest (identName ident) (Right ())
      _                  -> checkUses env s >> go env rest

    -- add a fresh local after validating its initializer against the vars in
    -- scope *before* it (so `int a = a;` and a redeclaration are rejected)
    declare env rest v initOk =
      if v `M.member` env
        then Left $ "duplicate declaration of variable '" ++ v ++ "'"
        else initOk >> go (M.insert v (-4 * (M.size env + 1)) env) rest

    -- every variable referenced by `node` must already be in scope
    checkUses env node =
      case filter (`M.notMember` env) (referenced node) of
        []      -> Right ()
        (v : _) -> Left $ "undeclared variable '" ++ v ++ "'"

-- every identifier appearing anywhere in `node` (a use, or an assignment
-- target -- both must be declared)
referenced :: Data a => a -> [String]
referenced = map nameOf . listify isName
  where isName (Name _ _) = True
        isName _          = False
        nameOf (Name _ s) = s
        nameOf _          = ""

identName :: Ident -> String
identName (Name _ s) = s
identName other = error $ "resolve: unexpected identifier node: " ++ show other
