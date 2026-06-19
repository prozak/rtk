{-# LANGUAGE QuasiQuotes #-}

-- Stages 1-3 code generation: C AST -> assembly AST. Both sides of the
-- translation are RTK-generated: the input is destructured with CQQ
-- quasi-quotation patterns, the output is built with AsmQQ construction
-- quotes and $-antiquote splices.
--
-- Token payloads (the integer literal, the function name) cannot be bound or
-- spliced by an antiquote ($x works on whole syntax sorts only), so leaf
-- nodes go through the grammar's named constructors: matching IntLit/Name to
-- read a payload, mkImm/mkSym to build an assembly leaf (positioned with
-- rtkNoPos; AST equality ignores positions by design).
module Codegen (codegen) where

import Prelude hiding (exp) -- the Exp quoter is named exp, like Prelude.exp

import CParser hiding (rtkNoPos) -- both parsers export rtkNoPos; we need AsmParser's
import CQQ

import AsmParser
import AsmQQ

codegen :: Program -> Asm
codegen [program| int $name ( ) { $stmts } |] =
  let sym = mkSym (identName name)
      items =
        concatMap genStatement stmts
          -- C99 5.1.2.2.3: falling off the end of main returns 0
          ++ [asmItems|
               movl $0, %eax
               ret
             |]
  in -- note the space in `$sym :` -- an antiquote name directly followed by
     -- ':' reads as the explicit $Rule:name antiquote form instead
     [asm|
       .globl $sym
       $sym :
       $items
     |]
codegen other = error $ "codegen: unsupported program: " ++ show other

genStatement :: Statement -> [AsmItem]
genStatement [statement| return $e ; |] = genExp e ++ [asmItems| ret |]
genStatement other = error $ "codegen: unsupported statement: " ++ show other

-- Evaluate an expression, leaving its value in %eax. One QQ pattern per
-- precedence level matches the corresponding node: the cascade put Add and
-- Mul in the same Exp type, so [exp| $e1 $aop $e2 |] and [exp| $e1 $mop $e2 |]
-- pick them apart by the operator's sort.
genExp :: Exp -> [AsmItem]
genExp [exp| $e1 $aop $e2 |] = genBinary e1 e2 (applyAddOp aop)
genExp [exp| $e1 $mop $e2 |] = genBinary e1 e2 (applyMulOp mop)
genExp [exp| $op1 $e1 |]     = genExp e1 ++ genUnaryOp op1
genExp (IntLit _ n)          = [asmItems| movl $src, %eax |]
  where src = mkImm n
genExp other = error $ "codegen: unsupported expression: " ++ show other

-- A binary operator. Evaluate the right operand and push it, evaluate the left
-- into %eax, pop the right into %ecx, then apply with the left in %eax. Doing
-- the right operand first is what leaves the left in %eax, which is where subl
-- and idivl need it. push/pop are 64-bit (%rax/%rcx); the value rides in the
-- low half.
genBinary :: Exp -> Exp -> [AsmItem] -> [AsmItem]
genBinary e1 e2 apply =
  genExp e2 ++ [asmItems| push %rax |]
            ++ genExp e1 ++ [asmItems| pop %rcx |]
            ++ apply

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

-- Apply a unary operator to the value already in %eax. The operators are
-- payload-free leaves, so they are matched by their named constructors rather
-- than quasi-quotes (the structured Exp above is what QQ is good for).
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

-- C leaf destructor

identName :: Ident -> String
identName (Name _ s) = s
identName other = error $ "codegen: unexpected identifier node: " ++ show other
