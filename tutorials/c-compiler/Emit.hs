{-# LANGUAGE QuasiQuotes #-}

-- Assembly AST -> AT&T-syntax text. RTK generates parsers, not
-- pretty-printers, so this is the hand-written half of the assembly round
-- trip; TestQQ checks emit/parse consistency (parsing emitted text yields
-- the original AST - positions differ, but AST equality ignores them).
module Emit (emit) where

import AsmParser
import AsmQQ

emit :: Asm -> String
emit [asm| $items |] = unlines (map emitItem items)
emit other = error $ "emit: unsupported assembly: " ++ show other

emitItem :: AsmItem -> String
-- the space before ':' is antiquote syntax only ($sym: would read as an
-- explicit $Rule:name antiquote); emitted labels use the conventional main:
emitItem [asmItem| .globl $sym |] = "    .globl " ++ symName sym
emitItem [asmItem| $sym : |] = symName sym ++ ":"
emitItem [asmItem| movl $src, $dst |] = "    movl    " ++ binOperands src dst
emitItem [asmItem| negl $dst |] = "    negl    " ++ emitOperand dst
emitItem [asmItem| notl $dst |] = "    notl    " ++ emitOperand dst
emitItem [asmItem| cmpl $src, $dst |] = "    cmpl    " ++ binOperands src dst
emitItem [asmItem| sete $dst |] = "    sete    " ++ emitOperand dst
emitItem [asmItem| addl $src, $dst |] = "    addl    " ++ binOperands src dst
emitItem [asmItem| subl $src, $dst |] = "    subl    " ++ binOperands src dst
emitItem [asmItem| imull $src, $dst |] = "    imull   " ++ binOperands src dst
emitItem [asmItem| cdq |] = "    cdq"
emitItem [asmItem| idivl $dst |] = "    idivl   " ++ emitOperand dst
emitItem [asmItem| push $dst |] = "    push    " ++ emitOperand dst
emitItem [asmItem| pop $dst |] = "    pop     " ++ emitOperand dst
emitItem [asmItem| ret |] = "    ret"
emitItem other = error $ "emitItem: unsupported item: " ++ show other

binOperands :: Operand -> Operand -> String
binOperands src dst = emitOperand src ++ ", " ++ emitOperand dst

emitOperand :: Operand -> String
emitOperand (Imm _ n) = "$" ++ show n
emitOperand (RegOp _ r) = emitReg r
emitOperand other = error $ "emitOperand: unsupported operand: " ++ show other

-- The register set grew from one to five, so render it by named constructor
-- rather than a quasi-quote per register.
emitReg :: Reg -> String
emitReg (Eax _) = "%eax"
emitReg (Al _)  = "%al"
emitReg (Ecx _) = "%ecx"
emitReg (Rax _) = "%rax"
emitReg (Rcx _) = "%rcx"
emitReg other = error $ "emitReg: unsupported register: " ++ show other

symName :: AsmId -> String
symName (Sym _ s) = s
symName other = error $ "symName: unexpected symbol: " ++ show other
