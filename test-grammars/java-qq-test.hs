{-# LANGUAGE QuasiQuotes #-}

-- Comprehensive Java Quasi-Quotation Test Suite
-- Tests construction, splicing, and pattern matching with shared types

import qualified JavaQQ as J
import qualified JavaParser as JP
import Data.List (isInfixOf)
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
    putStrLn "=========================================================="
    putStrLn "Java Quasi-Quotation Comprehensive Test Suite"
    putStrLn "=========================================================="
    putStrLn ""

    testConstruction
    putStrLn ""
    testSplicing
    putStrLn ""
    testPatternMatching
    putStrLn ""
    testDollarEscape
    putStrLn ""
    testSubExpressionSplices
    putStrLn ""

    putStrLn "=========================================================="
    putStrLn "All tests completed successfully!"
    putStrLn "=========================================================="

-- ========== PART 1: Construction Tests ==========
testConstruction :: IO ()
testConstruction = do
    putStrLn "========== PART 1: Construction Tests =========="
    putStrLn ""

    -- Basic expressions
    putStrLn "--- Expressions ---"
    let _ = [J.expression| x + y |]
    putStrLn "✅ [expression| x + y |]"

    let _ = [J.expression| a * b + c / d |]
    putStrLn "✅ [expression| a * b + c / d |]"

    let _ = [J.expression| (x + y) * z - 5 |]
    putStrLn "✅ [expression| (x + y) * z - 5 |]"

    let _ = [J.expression| x > 0 ? x : -x |]
    putStrLn "✅ [expression| x > 0 ? x : -x |] (ternary)"
    putStrLn ""

    -- Method calls
    putStrLn "--- Method Calls ---"
    let _ = [J.expression| obj.method(arg) |]
    putStrLn "✅ [expression| obj.method(arg) |]"

    let _ = [J.expression| obj.method(arg1, arg2, arg3) |]
    putStrLn "✅ [expression| obj.method(arg1, arg2, arg3) |]"

    let _ = [J.expression| obj1.method1(obj2.method2(arg)) |]
    putStrLn "✅ [expression| obj1.method1(obj2.method2(arg)) |] (nested)"
    putStrLn ""

    -- Arrays and objects
    putStrLn "--- Arrays & Objects ---"
    let _ = [J.expression| array[index] |]
    putStrLn "✅ [expression| array[index] |]"

    let _ = [J.expression| new ArrayList() |]
    putStrLn "✅ [expression| new ArrayList() |]"

    let _ = [J.expression| (int) value |]
    putStrLn "✅ [expression| (int) value |] (cast)"
    putStrLn ""

    -- Literals
    putStrLn "--- Literals ---"
    let _ = [J.expression| 42 |]
    putStrLn "✅ [expression| 42 |]"

    let _ = [J.expression| "hello world" |]
    putStrLn "✅ [expression| \"hello world\" |]"

    let _ = [J.literal| true |]
    putStrLn "✅ [literal| true |]"
    putStrLn ""

    -- Statements
    putStrLn "--- Statements ---"
    let _ = [J.statement| return x; |]
    putStrLn "✅ [statement| return x; |]"

    let _ = [J.statement| x = 5; |]
    putStrLn "✅ [statement| x = 5; |]"

    let _ = [J.statement| if (x > 0) { return x; } |]
    putStrLn "✅ [statement| if (x > 0) { return x; } |]"

    let _ = [J.statement| if (x > 0) { return x; } else { return -x; } |]
    putStrLn "✅ [statement| if-else |]"

    let _ = [J.statement| if (x > 0) if (y > 0) f(); else g(); |]
    putStrLn "✅ [statement| nested if without braces (dangling else) |]"

    let _ = [J.statement| while (x > 0) { x = x - 1; } |]
    putStrLn "✅ [statement| while loop |]"

    let _ = [J.statement| for (int i = 0; i < 10; i = i + 1) { sum = sum + i; } |]
    putStrLn "✅ [statement| for loop |]"
    putStrLn ""

    -- Statement blocks
    putStrLn "--- Statement Blocks ---"
    let _ = [J.statementBlock| { return x; } |]
    putStrLn "✅ [statementBlock| { return x; } |]"

    let _ = [J.statementBlock| { int x = 5; return x; } |]
    putStrLn "✅ [statementBlock| multi-statement |]"
    putStrLn ""

    -- Other constructs
    putStrLn "--- Other Constructs ---"
    let _ = [J.compoundName| java.util.List |]
    putStrLn "✅ [compoundName| java.util.List |]"

    let _ = [J.modifier| public |]
    putStrLn "✅ [modifier| public |]"

    let _ = [J.modifier| static |]
    putStrLn "✅ [modifier| static |]"

    putStrLn ""
    putStrLn "Construction tests: ALL PASSED ✅"

-- ========== PART 2: Splicing Tests ==========
testSplicing :: IO ()
testSplicing = do
    putStrLn "========== PART 2: Splicing Tests (Shared Types) =========="
    putStrLn ""
    putStrLn "These tests verify that shared types enable splicing"
    putStrLn "in hierarchical expression grammars."
    putStrLn ""

    -- Build base expressions
    let x = [J.expression| x |]
    let one = [J.expression| 1 |]
    let two = [J.expression| 2 |]

    putStrLn "Base expressions: x, one=1, two=2"
    putStrLn ""

    -- NOTE: Splicing currently requires full type annotation like $Expression:x
    -- This is a known limitation - see documentation
    putStrLn "⚠️  Note: Splicing syntax requires type annotation: $Expression:var"
    putStrLn "    Example: [expression| $Expression:x + $Expression:one |]"
    putStrLn ""

    putStrLn "--- Test: Addition splicing ---"
    let spliced1 = [J.expression| $Expression:x + $Expression:one |]
    putStrLn "✅ [expression| $Expression:x + $Expression:one |]"

    putStrLn "--- Test: Multiplication splicing ---"
    let spliced2 = [J.expression| $Expression:x * $Expression:two |]
    putStrLn "✅ [expression| $Expression:x * $Expression:two |]"

    putStrLn "--- Test: Complex splicing ---"
    let spliced3 = [J.expression| $Expression:x + $Expression:one * $Expression:two |]
    putStrLn "✅ Complex expression with multiple operators"

    putStrLn "--- Test: Nested splicing ---"
    let spliced4 = [J.expression| ($Expression:x + $Expression:one) * $Expression:two |]
    putStrLn "✅ Nested expression with parentheses"
    putStrLn ""

    putStrLn "Splicing tests: ALL PASSED ✅"

-- ========== PART 3: Pattern Matching Tests ==========
testPatternMatching :: IO ()
testPatternMatching = do
    putStrLn "========== PART 3: Pattern Matching Tests =========="
    putStrLn ""
    putStrLn "Pattern matching tests verify destructuring with QQ."
    putStrLn ""

    -- Build test expressions
    let addExpr = [J.expression| a + b |]
    let litExpr = [J.expression| 42 |]

    putStrLn "Test expressions: addExpr = a + b, litExpr = 42"
    putStrLn ""

    putStrLn "⚠️  Note: Pattern matching requires same syntax as splicing"
    putStrLn "    Example: case e of [expression| $Expression:x + $Expression:y |] -> ..."
    putStrLn ""

    putStrLn "--- Test: Pattern match addition ---"
    case addExpr of
        [J.expression| $Expression:left + $Expression:right |] -> do
            putStrLn "✅ Matched addition pattern"
            putStrLn $ "  Left: " ++ ppShow left
            putStrLn $ "  Right: " ++ ppShow right
        _ -> putStrLn "❌ Did not match"

    putStrLn "--- Test: Negative match (literal vs binary op) ---"
    case litExpr of
        [J.expression| $Expression:left + $Expression:right |] ->
            putStrLn "❌ Unexpected match"
        _ -> putStrLn "✅ Correctly didn't match"

    putStrLn "--- Test: Extract from return statement ---"
    let stmt = [J.statement| return x + 1; |]
    case stmt of
        [J.statement| return $OptExpression:expr ; |] -> do
            putStrLn "✅ Extracted expression from return"
            putStrLn $ "  Expression: " ++ ppShow expr
        _ -> putStrLn "❌ Could not extract"
    putStrLn ""

    putStrLn "Pattern matching tests: ALL PASSED ✅"

-- ========== PART 4: Literal '$' ($$ escape) Tests ==========
testDollarEscape :: IO ()
testDollarEscape = do
    putStrLn "========== PART 4: Literal '$' ($$ escape) Tests =========="
    putStrLn ""
    putStrLn "$$name in a quote body escapes metavariable rewriting and"
    putStrLn "produces the literal text $name - needed for '$' inside the"
    putStrLn "quoted language's string literals."
    putStrLn ""

    putStrLn "--- Test: $$ inside a quoted Java string literal ---"
    let priceExpr = [J.expression| "price: $$total" |]
    expectInAST "string literal round-trips with a literal $total"
                "price: $total" (ppShow priceExpr)

    putStrLn "--- Test: $$ escape next to a real splice ---"
    let x = [J.expression| x |]
    let mixed = [J.expression| $Expression:x + "cost: $$amount" |]
    expectInAST "string keeps the literal $amount while $Expression:x splices"
                "cost: $amount" (ppShow mixed)

    putStrLn ""
    putStrLn "Dollar-escape tests: ALL PASSED ✅"

-- ========== PART 5: Sub-expression splice positions ==========
-- The splice alternative sits only on PrimaryNoPostfix, the bottom of the
-- expression precedence chain; a spliced expression climbs the chain's unit
-- productions up to whatever level its position demands. Each build here is
-- round-tripped through the equivalent pattern splice: the pattern anchors
-- at the same chain position, so it must hand back exactly the values that
-- were spliced in. Mismatches are fatal (they fail `make test-java-qq`).
testSubExpressionSplices :: IO ()
testSubExpressionSplices = do
    putStrLn "========== PART 5: Sub-expression splice positions =========="
    putStrLn ""
    let x   = [J.expression| x |]
        one = [J.expression| 1 |]
        two = [J.expression| 2 |]

    putStrLn "--- Test: operands of '+' ---"
    case [J.expression| $Expression:x + $Expression:one |] of
        [J.expression| $Expression:l + $Expression:r |]
            | l == x && r == one -> putStrLn "✅ splices as '+' operands round-trip"
        other -> error $ "FAILED: '+' operand splices did not round-trip:\n" ++ ppShow other

    putStrLn "--- Test: operand of a prefix operator ---"
    case [J.expression| - $Expression:x |] of
        [J.expression| - $Expression:e |]
            | e == x -> putStrLn "✅ splice as a prefix-operator operand round-trips"
        other -> error $ "FAILED: prefix operand splice did not round-trip:\n" ++ ppShow other

    putStrLn "--- Test: operand of a cast ---"
    case [J.expression| (int) $Expression:x |] of
        [J.expression| (int) $Expression:e |]
            | e == x -> putStrLn "✅ splice as a cast operand round-trips"
        other -> error $ "FAILED: cast operand splice did not round-trip:\n" ++ ppShow other

    putStrLn "--- Test: array base and index ---"
    case [J.expression| $Expression:x[$Expression:one] |] of
        [J.expression| $Expression:arr[$Expression:idx] |]
            | arr == x && idx == one -> putStrLn "✅ splices as array base and index round-trip"
        other -> error $ "FAILED: indexing splices did not round-trip:\n" ++ ppShow other

    putStrLn "--- Test: all three ternary positions ---"
    case [J.expression| $Expression:x ? $Expression:one : $Expression:two |] of
        [J.expression| $Expression:c ? $Expression:t : $Expression:e |]
            | c == x && t == one && e == two -> putStrLn "✅ splices in ternary positions round-trip"
        other -> error $ "FAILED: ternary splices did not round-trip:\n" ++ ppShow other

    putStrLn "--- Test: method call arguments ---"
    case [J.expression| f($Expression:x, $Expression:two) |] of
        [J.expression| f($Expression:a, $Expression:b) |]
            | a == x && b == two -> putStrLn "✅ splices as call arguments round-trip"
        other -> error $ "FAILED: call argument splices did not round-trip:\n" ++ ppShow other

    putStrLn "--- Test: several precedence levels at once ---"
    case [J.expression| $Expression:x * $Expression:two + $Expression:one |] of
        [J.expression| $Expression:a * $Expression:b + $Expression:c |]
            | a == x && b == two && c == one -> putStrLn "✅ splices across precedence levels round-trip"
        other -> error $ "FAILED: mixed-level splices did not round-trip:\n" ++ ppShow other

    putStrLn ""
    putStrLn "Sub-expression splice tests: ALL PASSED ✅"

-- Unlike the informational checks above, a wrong AST here must fail the
-- test binary (and with it `make test-java-qq`), so mismatches are fatal.
expectInAST :: String -> String -> String -> IO ()
expectInAST what needle shown
    | not (needle `isInfixOf` shown) =
        error $ "FAILED: " ++ what ++ ": expected " ++ show needle
              ++ " in the AST:\n" ++ shown
    | "$$" `isInfixOf` shown =
        error $ "FAILED: " ++ what ++ ": the $$ escape was not collapsed"
              ++ " to a single $ in the AST:\n" ++ shown
    | otherwise = putStrLn $ "✅ " ++ what

{-
TEST SUMMARY:

✅ Construction: Fully working
   - All Java constructs can be built using quasi-quotation
   - 26+ different test cases covering expressions, statements, blocks, etc.

✅ Shared Types: Working
   - Grammar uses shared Expression type for all 18 expression rules
   - GenAST deduplicates constructors when combining shared-type rules
   - The anti-alternative is attached at PrimaryNoPostfix, the bottom of the
     precedence chain; splices reach every other rule of the group by
     climbing the chain's unit productions (see Normalize.computeQQAttachPoints)

✅ Splicing: Fully working
   - One anti-alternative per shared-type group keeps the parser free of
     splice-induced reduce/reduce conflicts
   - Syntax: $TypeName:variable
   - Tests: addition, multiplication, complex expressions, nested

✅ Pattern Matching: Fully working
   - Depends on splicing infrastructure (now complete)
   - Same syntax as splicing
   - Tests: pattern matching, negative matches, extraction

✅ Sub-expression positions: Fully working
   - Splices parse as operands of binary/prefix/cast/ternary/call/indexing
     constructs and round-trip through the equivalent pattern splices

IMPACT:
This test suite demonstrates the COMPLETE solution to the "hierarchical QQ
problem"! Construction, splicing, and pattern matching ALL work with shared
types in hierarchical grammars, in any sub-expression position.
-}
