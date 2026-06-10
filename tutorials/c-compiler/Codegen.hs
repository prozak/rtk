{-# LANGUAGE QuasiQuotes #-}

-- Stage 1 code generation: C AST -> assembly AST. Both sides of the
-- translation are RTK-generated: the input is destructured with CQQ
-- quasi-quotation patterns, the output is built with AsmQQ construction
-- quotes and $-antiquote splices.
--
-- Token payloads (the integer literal, the function name) cannot be bound
-- or spliced by an antiquote ($x works on whole syntax sorts only), so leaf
-- nodes go through the generated constructors: expValue/identName on the C
-- side, mkImm/mkSym on the assembly side (positioned with rtkNoPos; AST
-- equality ignores positions by design).
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
genStatement [statement| return $e ; |] =
  let src = mkImm (expValue e)
  in [asmItems|
       movl $src, %eax
       ret
     |]
genStatement other = error $ "codegen: unsupported statement: " ++ show other

-- assembly leaf constructors

mkImm :: Int -> Operand
mkImm = Ctr__Operand__0 rtkNoPos

mkSym :: String -> AsmId
mkSym = Ctr__AsmId__0 rtkNoPos

-- C leaf destructors

identName :: Ident -> String
identName (Ctr__Ident__0 _ s) = s
identName other = error $ "codegen: unexpected identifier node: " ++ show other

expValue :: Exp -> Int
expValue (Ctr__Exp__0 _ n) = n
expValue other = error $ "codegen: unexpected expression node: " ++ show other
