{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
--import Control.Exception(bracket)
import PLexer
import PParser
import PQQ
import Text.Show.Pretty

subst :: Id -> E -> Int -> E
subst id [e|(fold $e1 $e2 (lambda ( $id1 $id2 ) $e3))|] i =
  let e1 = subst id e1 i
      e2 = subst id e2 i
      e3 = subst id e3 i
    in [e|(fold $e1 $e2 (lambda ( $id1 $id2 ) $e3))|]

-- subst id [e|$id1|] i = if 

evalE :: E -> Int
evalE _ = 0

evalP :: P -> Int -> Int
evalP [p|(lambda ($id) $e)|] i = evalE $ subst id e i

main = do
    let _p = parseP $ alexScanTokens "(lambda (x) (fold x 0 (lambda (y z) (or y z))))"
    let [p|(lambda ($id) $e)|] = _p
    putStrLn $ show $ subst (Ctr__Id__0 "x") e 0x1122334455667788
    return ()
