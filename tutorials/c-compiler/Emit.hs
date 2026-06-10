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
emitItem [asmItem| ret |] = "    ret"
emitItem other = error $ "emitItem: unsupported item: " ++ show other

emitOperand :: Operand -> String
emitOperand [operand| %eax |] = "%eax"
emitOperand (Ctr__Operand__0 _ n) = "$" ++ show n
emitOperand other = error $ "emitOperand: unsupported operand: " ++ show other

symName :: AsmId -> String
symName (Ctr__AsmId__0 _ s) = s
symName other = error $ "symName: unexpected symbol: " ++ show other
