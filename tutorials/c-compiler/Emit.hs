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
emitItem [asmItem| movl $src, $dst |] =
  "    movl    " ++ emitOperand src ++ ", " ++ emitOperand dst
emitItem [asmItem| negl $dst |] = "    negl    " ++ emitOperand dst
emitItem [asmItem| notl $dst |] = "    notl    " ++ emitOperand dst
emitItem [asmItem| cmpl $src, $dst |] =
  "    cmpl    " ++ emitOperand src ++ ", " ++ emitOperand dst
emitItem [asmItem| sete $dst |] = "    sete    " ++ emitOperand dst
emitItem [asmItem| ret |] = "    ret"
emitItem other = error $ "emitItem: unsupported item: " ++ show other

emitOperand :: Operand -> String
emitOperand [operand| %eax |] = "%eax"
emitOperand [operand| %al |] = "%al"
emitOperand (Imm _ n) = "$" ++ show n
emitOperand other = error $ "emitOperand: unsupported operand: " ++ show other

symName :: AsmId -> String
symName (Sym _ s) = s
symName other = error $ "symName: unexpected symbol: " ++ show other
