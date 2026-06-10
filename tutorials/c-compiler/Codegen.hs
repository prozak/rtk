{-# LANGUAGE QuasiQuotes #-}

-- Stage 1 code generation: x86-64 assembly, AT&T syntax, System V ABI.
--
-- The AST is taken apart with the RTK-generated quasi-quoters from CQQ
-- ([program| ... |], [stmt-level patterns, $-antiquotes). Token payloads
-- (the integer literal, the function name) cannot be bound by an antiquote
-- ($x splices/matches whole syntax sorts only), so leaf nodes are taken
-- apart with their generated constructors instead.
module Codegen (codegen) where

import Prelude hiding (exp) -- the Exp quoter is named exp, like Prelude.exp

import CParser
import CQQ

codegen :: Program -> String
codegen [program| int $name ( ) { $stmts } |] =
  unlines $
    [ "    .globl " ++ fname
    , fname ++ ":"
    ]
      ++ concatMap genStatement stmts
      -- C99 5.1.2.2.3: falling off the end of main returns 0
      ++ [ "    movl    $0, %eax"
         , "    ret"
         ]
  where
    fname = identName name
codegen other = error $ "codegen: unsupported program: " ++ show other

genStatement :: Statement -> [String]
genStatement [statement| return $e ; |] =
  [ "    movl    $" ++ show (expValue e) ++ ", %eax"
  , "    ret"
  ]
genStatement other = error $ "codegen: unsupported statement: " ++ show other

identName :: Ident -> String
identName (Ctr__Ident__0 s) = s
identName other = error $ "codegen: unexpected identifier node: " ++ show other

expValue :: Exp -> Int
expValue (Ctr__Exp__0 n) = n
expValue other = error $ "codegen: unexpected expression node: " ++ show other
