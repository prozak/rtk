{-# LANGUAGE QuasiQuotes #-}

-- PL/0 Quasi-Quotation Test Suite
--
-- Exercises the full quasi-quotation feature set on the PL/0 grammar:
-- construction, anti-quotation (splicing), pattern matching with
-- metavariables, and AST rewrite rules. The cg* functions below are a
-- miniature of a PL/0 -> C code generator written entirely against QQ
-- patterns; auto-generated constructor names appear only for leaf
-- unwrapping (Ident/Number), the statement-list of 'begin', and the
-- Program/Block shells. Positions (RtkPos) are equality-transparent
-- and become wildcards in QQ patterns, so they never get in the way.
--
-- Metavariable naming: a $var must start with a shortcut declared in
-- pl0.pg ($e* Expression, $s* Statement, $so* StatementOpt,
-- $c* Condition, $id* Ident, $n* Number) or with a type name
-- ($statementOpt1, ...); see docs/why-qq-limitations.md.

import Pl0Lexer
import Pl0Parser
import Pl0QQ
import Data.Generics (everywhere, mkT)
import Control.Monad (unless)
import System.Exit (exitFailure)

-- ===== miniature code generator: QQ patterns all the way down =====

cgStmt :: Statement -> String
cgStmt [statement| $id1 := $e1 |]       = cgIdent id1 ++ " = " ++ cgExpr e1 ++ ";"
cgStmt [statement| call $id1 |]         = cgIdent id1 ++ "();"
cgStmt [statement| if $c1 then $so1 |]  = "if " ++ cgCond c1 ++ " " ++ cgStmtOpt so1
cgStmt [statement| while $c1 do $so1 |] = "while " ++ cgCond c1 ++ " " ++ cgStmtOpt so1
cgStmt (Ctr__Statement__2 _ sts)        = "{ " ++ unwords (map cgStmtOpt sts) ++ " }"
cgStmt other = error ("cgStmt: unhandled statement: " ++ show other)

-- Wirth's statement is optional everywhere it appears; the empty
-- statement compiles to C's empty statement
cgStmtOpt :: StatementOpt -> String
cgStmtOpt [statementOpt| |]     = ";"
cgStmtOpt [statementOpt| $s1 |] = cgStmt s1
cgStmtOpt other = error ("cgStmtOpt: unhandled statement: " ++ show other)

cgCond :: Condition -> String
cgCond [condition| odd $e1 |]    = "(" ++ cgExpr e1 ++ " % 2 != 0)"
cgCond [condition| $e1 = $e2 |]  = "(" ++ cgExpr e1 ++ " == " ++ cgExpr e2 ++ ")"
cgCond [condition| $e1 # $e2 |]  = "(" ++ cgExpr e1 ++ " != " ++ cgExpr e2 ++ ")"
cgCond [condition| $e1 < $e2 |]  = "(" ++ cgExpr e1 ++ " < " ++ cgExpr e2 ++ ")"
cgCond [condition| $e1 <= $e2 |] = "(" ++ cgExpr e1 ++ " <= " ++ cgExpr e2 ++ ")"
cgCond [condition| $e1 > $e2 |]  = "(" ++ cgExpr e1 ++ " > " ++ cgExpr e2 ++ ")"
cgCond [condition| $e1 >= $e2 |] = "(" ++ cgExpr e1 ++ " >= " ++ cgExpr e2 ++ ")"
cgCond other = error ("cgCond: unhandled condition: " ++ show other)

cgExpr :: Expression -> String
cgExpr [expression| $e1 + $e2 |] = "(" ++ cgExpr e1 ++ " + " ++ cgExpr e2 ++ ")"
cgExpr [expression| $e1 - $e2 |] = "(" ++ cgExpr e1 ++ " - " ++ cgExpr e2 ++ ")"
cgExpr [expression| $e1 * $e2 |] = "(" ++ cgExpr e1 ++ " * " ++ cgExpr e2 ++ ")"
cgExpr [expression| $e1 / $e2 |] = "(" ++ cgExpr e1 ++ " / " ++ cgExpr e2 ++ ")"
cgExpr [expression| - $e1 |]     = "(-" ++ cgExpr e1 ++ ")"
cgExpr [expression| + $e1 |]     = cgExpr e1
cgExpr [expression| $id1 |]      = cgIdent id1
cgExpr [expression| $n1 |]       = cgNum n1
cgExpr other = error ("cgExpr: unhandled expression: " ++ show other)

cgIdent :: Ident -> String
cgIdent (Ctr__Ident__0 _ name) = name
cgIdent (Anti_Ident v) = error ("cgIdent: unexpected anti node: " ++ v)

cgNum :: Number -> String
cgNum (Ctr__Number__0 _ i) = show i
cgNum (Anti_Number v) = error ("cgNum: unexpected anti node: " ++ v)

-- ===== AST rewrite rules: QQ patterns + QQ construction with SYB =====

simplify :: Expression -> Expression
simplify [expression| $e1 + 0 |] = e1
simplify [expression| 0 + $e1 |] = e1
simplify [expression| $e1 - 0 |] = e1
simplify [expression| $e1 * 1 |] = e1
simplify [expression| 1 * $e1 |] = e1
simplify x = x

optimize :: Expression -> Expression
optimize = everywhere (mkT simplify)

-- Statement lists: positional element metavariables work; a single
-- metavariable for the whole list is not supported for inline lists
-- (it would bind one element; see docs/why-qq-limitations.md)
isAssignPair :: Statement -> Bool
isAssignPair [statement| begin $s1 ; $s2 end |] =
    cgStmt s1 == "x = 1;" && cgStmt s2 == "y = x;"
isAssignPair _ = False

-- ===== test harness =====

check :: String -> String -> String -> IO Bool
check name expected actual
    | expected == actual = do
        putStrLn ("PASS: " ++ name)
        return True
    | otherwise = do
        putStrLn ("FAIL: " ++ name)
        putStrLn ("  expected: " ++ expected)
        putStrLn ("  actual:   " ++ actual)
        return False

checkBool :: String -> Bool -> IO Bool
checkBool name b = check name "True" (show b)

main :: IO ()
main = do
    putStrLn "PL/0 Quasi-Quotation Test Suite"
    putStrLn "==============================="

    let eLhs    = [expression| x + 1 |]
        eRhs    = [expression| y - 1 |]
        spliced = [expression| $eLhs * $eRhs |]

    results <- sequence
        [ -- Construction builds the documented AST shape, with correct
          -- precedence and no wrapper nodes for the Term/Factor chain;
          -- positions compare as equal by construction (RtkPos Eq)
          checkBool "construction: x + 2 * y has the expected AST"
              ([expression| x + 2 * y |] ==
               Ctr__Expression__6 rtkNoPos
                   (Ctr__Expression__0 rtkNoPos (Ctr__Ident__0 rtkNoPos "x"))
                   (Ctr__Expression__3 rtkNoPos
                       (Ctr__Expression__1 rtkNoPos (Ctr__Number__0 rtkNoPos 2))
                       (Ctr__Expression__0 rtkNoPos (Ctr__Ident__0 rtkNoPos "y"))))

          -- Anti-quotation: splicing sub-ASTs equals direct construction
        , checkBool "splice: ($e1 * $e2) == direct construction"
              (spliced == [expression| (x + 1) * (y - 1) |])

          -- Pattern matching with metavariables drives the code generator
        , check "codegen: spliced expression"
              "((x + 1) * (y - 1))" (cgExpr spliced)
        , check "codegen: unary minus and plus"
              "((-x) + y)" (cgExpr [expression| - x + y |])
        , check "codegen: statement"
              "while (x < 10) x = (x + 1);"
              (cgStmt [statement| while x < 10 do x := x + 1 |])
        , check "codegen: if with odd"
              "if (n % 2 != 0) res = (res * b);"
              (cgStmt [statement| if odd n then res := res * b |])
        , check "codegen: all relational operators"
              "(a == b) (a != b) (a < b) (a <= b) (a > b) (a >= b)"
              (unwords (map cgCond
                  [ [condition| a = b |], [condition| a # b |]
                  , [condition| a < b |], [condition| a <= b |]
                  , [condition| a > b |], [condition| a >= b |] ]))

          -- Wirth's empty statement, at its own optional position
        , check "codegen: empty statement"
              ";" (cgStmtOpt [statementOpt| |])
        , check "codegen: empty branch (if x = 0 then ;)"
              "if (x == 0) ;"
              (cgStmt [statement| if x = 0 then |])

        , checkBool "pattern: two-element begin block"
              (isAssignPair [statement| begin x := 1 ; y := x end |])

          -- Rewrite rules: QQ patterns on the left, QQ values on the right
        , check "rewrite: identity-element elimination"
              "(x + y)"
              (cgExpr (optimize [expression| (x + 0) * 1 + y * 1 |]))

          -- Spliced result of a rewrite into a new statement
        , let e9 = optimize [expression| (res + 0) * 1 |]
          in check "splice rewritten expression into statement"
              "out = res;" (cgStmt [statement| out := $e9 |])

          -- The same parser handles files at runtime (Either API);
          -- navigate the Program/Block shell and code-generate the body
        , let parsed = scanTokens "var x; begin x := 6 * 7; if odd x then x := 0 end."
                         >>= parsePl0
          in check "runtime parse -> codegen"
              "{ x = (6 * 7); if (x % 2 != 0) x = 0; }"
              (case parsed of
                   Right (Ctr__Program__12 _ (Ctr__Block__0 _ _ _ _ so)) ->
                       cgStmtOpt so
                   Right other -> "<unexpected program shape: " ++ show other ++ ">"
                   Left err -> "<parse failed: " ++ err ++ ">")
        ]

    putStrLn "==============================="
    let failed = length (filter not results)
    unless (failed == 0) $ do
        putStrLn (show failed ++ " test(s) failed")
        exitFailure
    putStrLn ("All " ++ show (length results) ++ " tests passed")
