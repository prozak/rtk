{-# LANGUAGE QuasiQuotes #-}

-- Stages 1-4 code generation: C AST -> assembly AST. Both sides of the
-- translation are RTK-generated: the input is destructured with CQQ
-- quasi-quotation patterns, the output is built with AsmQQ construction
-- quotes and $-antiquote splices.
--
-- Short-circuit && and || need fresh, unique jump labels, so generation runs
-- in a State Int that hands out label numbers (see `fresh`). The functions
-- that can emit labels are monadic (genProgram/genStatement/genExp/genBinary/
-- genAnd/genOr); the pure helpers (apply*/genUnaryOp/compareSet and the leaf
-- builders) stay outside the monad.
--
-- Token payloads (the integer literal, the function name) cannot be bound or
-- spliced by an antiquote ($x works on whole syntax sorts only), so leaf
-- nodes go through the grammar's named constructors: matching IntLit/Name to
-- read a payload, mkImm/mkSym/jmpTo/... to build an assembly leaf (positioned
-- with rtkNoPos; AST equality ignores positions by design).
module Codegen (codegen) where

import Prelude hiding (exp) -- the Exp quoter is named exp, like Prelude.exp
import Control.Monad.State (State, evalState, state)

import CParser hiding (rtkNoPos) -- both parsers export rtkNoPos; we need AsmParser's
import CQQ

import AsmParser
import AsmQQ

-- Label supply: a counter threaded through generation.
type Gen = State Int

fresh :: Gen Int
fresh = state (\n -> (n, n + 1))

codegen :: Program -> Asm
codegen prog = evalState (genProgram prog) 0

genProgram :: Program -> Gen Asm
genProgram [program| int $name ( ) { $stmts } |] = do
  body <- concat <$> mapM genStatement stmts
  let sym = mkSym (identName name)
      -- C99 5.1.2.2.3: falling off the end of main returns 0
      items = body ++ [asmItems|
                         movl $0, %eax
                         ret
                       |]
  return [asm|
           .globl $sym
           $sym :
           $items
         |]
genProgram other = error $ "codegen: unsupported program: " ++ show other

genStatement :: Statement -> Gen [AsmItem]
genStatement [statement| return $e ; |] = do
  e' <- genExp e
  return (e' ++ [asmItems| ret |])
genStatement other = error $ "codegen: unsupported statement: " ++ show other

-- Evaluate an expression, leaving its value in %eax. One QQ pattern per
-- precedence level picks the matching node out of the single Exp type; && and
-- || short-circuit, so they are handled apart from the value operators.
genExp :: Exp -> Gen [AsmItem]
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
-- into %ecx, then apply with the left in %eax. (Right-first leaves the left in
-- %eax, where subl and idivl need it.)
genBinary :: Exp -> Exp -> [AsmItem] -> Gen [AsmItem]
genBinary e1 e2 apply = do
  r <- genExp e2
  l <- genExp e1
  return (r ++ [asmItems| push %rax |] ++ l ++ [asmItems| pop %rcx |] ++ apply)

-- a && b: if a is 0 the result is 0 and b is never evaluated; otherwise the
-- result is (b != 0).
genAnd :: Exp -> Exp -> Gen [AsmItem]
genAnd e1 e2 = do
  n <- fresh
  let rhs = mkSym ("_and_rhs_" ++ show n)
      end = mkSym ("_and_end_" ++ show n)
  l <- genExp e1
  r <- genExp e2
  return $ l
    ++ [asmItems| cmpl $0, %eax |]
    ++ [jneTo rhs, jmpTo end, label rhs]   -- a != 0 -> evaluate b; else fall to end with %eax = 0
    ++ r
    ++ [asmItems| cmpl $0, %eax
                  movl $0, %eax
                  setne %al |]
    ++ [label end]

-- a || b: if a is nonzero the result is 1 and b is never evaluated; otherwise
-- the result is (b != 0).
genOr :: Exp -> Exp -> Gen [AsmItem]
genOr e1 e2 = do
  n <- fresh
  let rhs = mkSym ("_or_rhs_" ++ show n)
      end = mkSym ("_or_end_" ++ show n)
  l <- genExp e1
  r <- genExp e2
  return $ l
    ++ [asmItems| cmpl $0, %eax |]
    ++ [jeTo rhs]                          -- a == 0 -> evaluate b
    ++ [asmItems| movl $1, %eax |]         -- a != 0 -> result 1
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
-- cdq sign-extends %eax into %edx:%eax; idivl divides that by %ecx, leaving the
-- quotient in %eax
applyMulOp (Divide _) = [asmItems|
                          cdq
                          idivl %ecx
                        |]
applyMulOp other = error $ "codegen: unsupported multiplicative operator: " ++ show other

-- Apply a unary operator to the value already in %eax. Payload-free leaves, so
-- matched by named constructor rather than quasi-quote.
genUnaryOp :: UnaryOp -> [AsmItem]
genUnaryOp (Neg _)        = [asmItems| negl %eax |]
genUnaryOp (Complement _) = [asmItems| notl %eax |]
-- logical not: set %eax to 1 if the value was 0, else 0
genUnaryOp (Not _)        = [asmItems|
                              cmpl $0, %eax
                              movl $0, %eax
                              sete %al
                            |]
genUnaryOp other = error $ "codegen: unsupported unary operator: " ++ show other

-- assembly leaf constructors

mkImm :: Int -> Operand
mkImm = Imm rtkNoPos

mkSym :: String -> AsmId
mkSym = Sym rtkNoPos

-- jumps and label definitions: one AsmId field each, so built by constructor
jmpTo, jeTo, jneTo, label :: AsmId -> AsmItem
jmpTo = Jmp rtkNoPos
jeTo  = Je rtkNoPos
jneTo = Jne rtkNoPos
label = Label rtkNoPos

-- C leaf destructor

identName :: Ident -> String
identName (Name _ s) = s
identName other = error $ "codegen: unexpected identifier node: " ++ show other
