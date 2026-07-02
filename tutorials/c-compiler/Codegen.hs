{-# LANGUAGE QuasiQuotes #-}

-- Stages 1-5 code generation: C AST -> assembly AST. Both sides of the
-- translation are RTK-generated: the input is destructured with CQQ
-- quasi-quotation patterns, the output is built with AsmQQ construction
-- quotes and $-antiquote splices.
--
-- Generation runs in a monad carrying two things: a Reader of the
-- variable->offset map that the Resolve pass computed (so a variable reference
-- knows its stack slot), and a State Int handing out unique labels for
-- short-circuit jumps. The functions that need either are monadic; the pure
-- helpers (apply*/genUnaryOp/compareSet and the leaf builders) stay outside.
--
-- Token payloads (the integer literal, the variable/function name) cannot be
-- bound or spliced by an antiquote, so leaf nodes go through the named
-- constructors: matching IntLit/Name to read a payload, mkImm/mkMem/mkSym/...
-- to build an assembly leaf (positioned with rtkNoPos; AST equality ignores
-- positions by design).
module Codegen (codegen) where

import Prelude hiding (exp) -- the Exp quoter is named exp, like Prelude.exp
import Control.Monad.Reader (ReaderT, runReaderT, asks)
import Control.Monad.State (State, evalState, state)
import qualified Data.Map as M

import CParser hiding (rtkNoPos) -- both parsers export rtkNoPos; we need AsmParser's
import CQQ

import AsmParser
import AsmQQ

import Resolve (VarMap)

-- Reader: the per-function variable->offset map. State: the label counter.
type Gen = ReaderT VarMap (State Int)

fresh :: Gen Int
fresh = state (\n -> (n, n + 1))

-- the stack slot an identifier resolves to (Resolve guaranteed it is present)
offsetOf :: Ident -> Gen Operand
offsetOf ident = asks (mkMem . (M.! identName ident))

codegen :: VarMap -> Program -> Asm
codegen vm prog = evalState (runReaderT (genProgram prog) vm) 0

genProgram :: Program -> Gen Asm
genProgram [program| int $name ( ) { $stmts } |] = do
  frame <- asks frameSize
  body <- concat <$> mapM genStatement stmts
  let sym = mkSym (identName name)
      -- C99 5.1.2.2.3: falling off the end of main returns 0
      items = prologue frame ++ body ++ [asmItems| movl $0, %eax |] ++ epilogue
  return [asm|
           .globl $sym
           $sym :
           $items
         |]
genProgram other = error $ "codegen: unsupported program: " ++ show other

-- a 16-byte-aligned frame big enough for every local (4 bytes each)
frameSize :: VarMap -> Int
frameSize vm = ((4 * M.size vm + 15) `div` 16) * 16

-- set up the frame: save the caller's base pointer, point %rbp at this frame,
-- and carve out room for the locals (skipped when there are none)
prologue :: Int -> [AsmItem]
prologue n =
  [asmItems| push %rbp
             movq %rsp, %rbp |]
  ++ [Subq rtkNoPos (mkImm n) rspOp | n > 0]

-- tear the frame down; every return path ends here
epilogue :: [AsmItem]
epilogue = [asmItems| movq %rbp, %rsp
                      pop %rbp
                      ret |]

genStatement :: Statement -> Gen [AsmItem]
genStatement [statement| return $e ; |] = do
  e' <- genExp e
  return (e' ++ epilogue)
genStatement [statement| int $name = $e ; |] = do   -- declaration with initializer
  e' <- genExp e
  dst <- offsetOf name
  return (e' ++ [asmItems| movl %eax, $dst |])
genStatement [statement| int $name ; |] = return []  -- declaration, uninitialized: no code
genStatement [statement| $e ; |] = genExp e          -- expression statement: evaluate, drop %eax
genStatement other = error $ "codegen: unsupported statement: " ++ show other

-- Evaluate an expression, leaving its value in %eax.
genExp :: Exp -> Gen [AsmItem]
genExp [exp| $name = $e |] = do        -- assignment is an expression: store, keep value in %eax
  e' <- genExp e
  dst <- offsetOf name
  return (e' ++ [asmItems| movl %eax, $dst |])
genExp [exp| $name |] = do             -- variable reference: load from the stack slot
  src <- offsetOf name
  return [asmItems| movl $src, %eax |]
genExp [exp| $e1 || $e2 |]     = genOr e1 e2
genExp [exp| $e1 && $e2 |]     = genAnd e1 e2
genExp [exp| $e1 $eqop $e2 |]  = genBinary e1 e2 (applyEqOp eqop)
genExp [exp| $e1 $relop $e2 |] = genBinary e1 e2 (applyRelOp relop)
genExp [exp| $e1 $aop $e2 |]   = genBinary e1 e2 (applyAddOp aop)
genExp [exp| $e1 $mop $e2 |]   = genBinary e1 e2 (applyMulOp mop)
genExp [exp| $op1 $e1 |]       = do e' <- genExp e1; return (e' ++ genUnaryOp op1)
genExp (IntLit _ n)            = return [asmItems| movl $src, %eax |]
  where src = mkImm n
genExp other = error $ "codegen: unsupported expression: " ++ show other

-- A binary operator over two computed values (arithmetic or comparison).
-- Evaluate the right operand and push it, the left into %eax, pop the right
-- into %ecx, then apply with the left in %eax.
genBinary :: Exp -> Exp -> [AsmItem] -> Gen [AsmItem]
genBinary e1 e2 apply = do
  r <- genExp e2
  l <- genExp e1
  return (r ++ [asmItems| push %rax |] ++ l ++ [asmItems| pop %rcx |] ++ apply)

-- a && b: if a is 0 the result is 0 and b is never evaluated; otherwise (b != 0)
genAnd :: Exp -> Exp -> Gen [AsmItem]
genAnd e1 e2 = do
  n <- fresh
  let rhs = mkSym ("_and_rhs_" ++ show n)
      end = mkSym ("_and_end_" ++ show n)
  l <- genExp e1
  r <- genExp e2
  return $ l
    ++ [asmItems| cmpl $0, %eax |]
    ++ [jneTo rhs, jmpTo end, label rhs]
    ++ r
    ++ [asmItems| cmpl $0, %eax
                  movl $0, %eax
                  setne %al |]
    ++ [label end]

-- a || b: if a is nonzero the result is 1 and b is never evaluated; otherwise (b != 0)
genOr :: Exp -> Exp -> Gen [AsmItem]
genOr e1 e2 = do
  n <- fresh
  let rhs = mkSym ("_or_rhs_" ++ show n)
      end = mkSym ("_or_end_" ++ show n)
  l <- genExp e1
  r <- genExp e2
  return $ l
    ++ [asmItems| cmpl $0, %eax |]
    ++ [jeTo rhs]
    ++ [asmItems| movl $1, %eax |]
    ++ [jmpTo end, label rhs]
    ++ r
    ++ [asmItems| cmpl $0, %eax
                  movl $0, %eax
                  setne %al |]
    ++ [label end]

-- Comparison: compare left (%eax) with right (%ecx), zero %eax (mov preserves
-- the flags cmpl set), then set %al per the condition.
compareSet :: [AsmItem] -> [AsmItem]
compareSet setcc =
  [asmItems| cmpl %ecx, %eax
             movl $0, %eax |] ++ setcc

applyEqOp :: EqOp -> [AsmItem]
applyEqOp (Equal _)    = compareSet [asmItems| sete %al |]
applyEqOp (NotEqual _) = compareSet [asmItems| setne %al |]
applyEqOp other = error $ "codegen: unsupported equality operator: " ++ show other

applyRelOp :: RelOp -> [AsmItem]
applyRelOp (Lt _) = compareSet [asmItems| setl %al |]
applyRelOp (Le _) = compareSet [asmItems| setle %al |]
applyRelOp (Gt _) = compareSet [asmItems| setg %al |]
applyRelOp (Ge _) = compareSet [asmItems| setge %al |]
applyRelOp other = error $ "codegen: unsupported relational operator: " ++ show other

-- Apply with the left operand in %eax and the right in %ecx.
applyAddOp :: AddOp -> [AsmItem]
applyAddOp (Plus _)  = [asmItems| addl %ecx, %eax |]
applyAddOp (Minus _) = [asmItems| subl %ecx, %eax |]
applyAddOp other = error $ "codegen: unsupported additive operator: " ++ show other

applyMulOp :: MulOp -> [AsmItem]
applyMulOp (Times _)  = [asmItems| imull %ecx, %eax |]
applyMulOp (Divide _) = [asmItems|
                          cdq
                          idivl %ecx
                        |]
applyMulOp other = error $ "codegen: unsupported multiplicative operator: " ++ show other

-- Apply a unary operator to the value already in %eax.
genUnaryOp :: UnaryOp -> [AsmItem]
genUnaryOp (Neg _)        = [asmItems| negl %eax |]
genUnaryOp (Complement _) = [asmItems| notl %eax |]
genUnaryOp (Not _)        = [asmItems|
                              cmpl $0, %eax
                              movl $0, %eax
                              sete %al
                            |]
genUnaryOp other = error $ "codegen: unsupported unary operator: " ++ show other

-- assembly leaf constructors

mkImm :: Int -> Operand
mkImm = Imm rtkNoPos

mkMem :: Int -> Operand     -- a local at off(%rbp)
mkMem off = Mem rtkNoPos off (Rbp rtkNoPos)

rspOp :: Operand
rspOp = RegOp rtkNoPos (Rsp rtkNoPos)

mkSym :: String -> AsmId
mkSym = Sym rtkNoPos

jmpTo, jeTo, jneTo, label :: AsmId -> AsmItem
jmpTo = Jmp rtkNoPos
jeTo  = Je rtkNoPos
jneTo = Jne rtkNoPos
label = Label rtkNoPos

-- C leaf destructor

identName :: Ident -> String
identName (Name _ s) = s
identName other = error $ "codegen: unexpected identifier node: " ++ show other
