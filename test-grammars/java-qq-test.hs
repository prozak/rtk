{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import JavaQQ
import Text.Show.Pretty

-- Test quasi-quotations with simple Java examples
main = do
    putStrLn "=== Java QuasiQuoter Tests ==="
    putStrLn "Testing basic quasi-quotation functionality for Java grammar"

    -- Test 1: Simple expression
    putStrLn "\n--- Test 1: Simple Expression ---"
    let expr1 = [expression| x + y |]
    putStrLn $ "Expression [x + y]:"
    putStrLn $ ppShow expr1

    -- Test 2: Method call expression (single argument)
    putStrLn "\n--- Test 2: Method Call (single arg) ---"
    let expr2 = [expression| obj.method(arg) |]
    putStrLn $ "Expression [obj.method(arg)]:"
    putStrLn $ ppShow expr2

    -- Test 3: Simple literal
    putStrLn "\n--- Test 3: Literal ---"
    let expr3 = [expression| 42 |]
    putStrLn $ "Expression [42]:"
    putStrLn $ ppShow expr3

    -- Test 4: Simple statement - return
    putStrLn "\n--- Test 4: Return Statement ---"
    let stmt1 = [statement| return; |]
    putStrLn $ "Statement [return;]:"
    putStrLn $ ppShow stmt1

    -- Test 5: Simple statement - assignment
    putStrLn "\n--- Test 5: Assignment Statement ---"
    let stmt2 = [statement| x = 5; |]
    putStrLn $ "Statement [x = 5;]:"
    putStrLn $ ppShow stmt2

    -- Test 6: Multi-argument method call
    putStrLn "\n--- Test 6: Method Call (multiple args) ---"
    let expr4 = [expression| obj.method(arg1, arg2) |]
    putStrLn $ "Expression [obj.method(arg1, arg2)]:"
    putStrLn $ ppShow expr4

    -- Test 7: Multi-argument method call with three arguments
    putStrLn "\n--- Test 7: Method Call (three args) ---"
    let expr5 = [expression| obj.method(arg1, arg2, arg3) |]
    putStrLn $ "Expression [obj.method(arg1, arg2, arg3)]:"
    putStrLn $ ppShow expr5

    -- Test 8: Nested method calls
    putStrLn "\n--- Test 8: Nested Method Calls ---"
    let expr6 = [expression| obj1.method1(obj2.method2(arg)) |]
    putStrLn $ "Expression [obj1.method1(obj2.method2(arg))]:"
    putStrLn $ ppShow expr6

    -- Test 9: Complex expression with multiple operators
    putStrLn "\n--- Test 9: Complex Expression (multiple operators) ---"
    let expr7 = [expression| (x + y) * z - 5 |]
    putStrLn $ "Expression [(x + y) * z - 5]:"
    putStrLn $ ppShow expr7

    -- Test 10: Array access expression
    putStrLn "\n--- Test 10: Array Access ---"
    let expr8 = [expression| array[index] |]
    putStrLn $ "Expression [array[index]]:"
    putStrLn $ ppShow expr8

    -- Test 11: Ternary conditional expression
    putStrLn "\n--- Test 11: Ternary Conditional ---"
    let expr9 = [expression| x > 0 ? x : -x |]
    putStrLn $ "Expression [x > 0 ? x : -x]:"
    putStrLn $ ppShow expr9

    -- Test 12: New object instantiation
    putStrLn "\n--- Test 12: Object Instantiation ---"
    let expr10 = [expression| new ArrayList() |]
    putStrLn $ "Expression [new ArrayList()]:"
    putStrLn $ ppShow expr10

    -- Test 13: Cast expression
    putStrLn "\n--- Test 13: Cast Expression ---"
    let expr11 = [expression| (int) value |]
    putStrLn $ "Expression [(int) value]:"
    putStrLn $ ppShow expr11

    -- Test 14: String literal
    putStrLn "\n--- Test 14: String Literal ---"
    let expr12 = [expression| "hello world" |]
    putStrLn $ "Expression [\"hello world\"]:"
    putStrLn $ ppShow expr12

    -- Test 15: Statement block
    putStrLn "\n--- Test 15: Statement Block ---"
    let block1 = [statementBlock| { return x; } |]
    putStrLn $ "StatementBlock [{ return x; }]:"
    putStrLn $ ppShow block1

    -- Test 16: Statement block with multiple statements
    putStrLn "\n--- Test 16: Statement Block (multiple statements) ---"
    let block2 = [statementBlock| { int x = 5; return x; } |]
    putStrLn $ "StatementBlock [{ int x = 5; return x; }]:"
    putStrLn $ ppShow block2

    -- Test 17: If statement
    putStrLn "\n--- Test 17: If Statement ---"
    let stmt3 = [statement| if (x > 0) { return x; } |]
    putStrLn $ "Statement [if (x > 0) { return x; }]:"
    putStrLn $ ppShow stmt3

    -- Test 18: If-else statement
    putStrLn "\n--- Test 18: If-Else Statement ---"
    let stmt4 = [statement| if (x > 0) { return x; } else { return -x; } |]
    putStrLn $ "Statement [if (x > 0) { return x; } else { return -x; }]:"
    putStrLn $ ppShow stmt4

    -- Test 19: While loop
    putStrLn "\n--- Test 19: While Loop ---"
    let stmt5 = [statement| while (x > 0) { x = x - 1; } |]
    putStrLn $ "Statement [while (x > 0) { x = x - 1; }]:"
    putStrLn $ ppShow stmt5

    -- Test 20: For loop
    putStrLn "\n--- Test 20: For Loop ---"
    let stmt6 = [statement| for (int i = 0; i < 10; i = i + 1) { sum = sum + i; } |]
    putStrLn $ "Statement [for (int i = 0; i < 10; i = i + 1) { sum = sum + i; }]:"
    putStrLn $ ppShow stmt6

    -- Test 21: Compound name (qualified)
    putStrLn "\n--- Test 21: Compound Name ---"
    let name1 = [compoundName| java.util.List |]
    putStrLn $ "CompoundName [java.util.List]:"
    putStrLn $ ppShow name1

    -- Test 22: Modifier - public
    putStrLn "\n--- Test 22: Modifier (public) ---"
    let mod1 = [modifier| public |]
    putStrLn $ "Modifier [public]:"
    putStrLn $ ppShow mod1

    -- Test 23: Modifier - static
    putStrLn "\n--- Test 23: Modifier (static) ---"
    let mod2 = [modifier| static |]
    putStrLn $ "Modifier [static]:"
    putStrLn $ ppShow mod2

    -- Test 24: Literal - string
    putStrLn "\n--- Test 24: Literal (string) ---"
    let lit1 = [literal| "hello" |]
    putStrLn $ "Literal [\"hello\"]:"
    putStrLn $ ppShow lit1

    -- Test 25: Literal - integer
    putStrLn "\n--- Test 25: Literal (integer) ---"
    let lit2 = [literal| 123 |]
    putStrLn $ "Literal [123]:"
    putStrLn $ ppShow lit2

    -- Test 26: Literal - boolean
    putStrLn "\n--- Test 26: Literal (boolean) ---"
    let lit3 = [literal| true |]
    putStrLn $ "Literal [true]:"
    putStrLn $ ppShow lit3

    putStrLn "\n=== All QuasiQuoter tests completed successfully! ==="
    putStrLn $ "\nTotal tests run: 26"
    putStrLn $ ""
    putStrLn $ "Test coverage:"
    putStrLn $ "  - Basic expressions (Tests 1-3)"
    putStrLn $ "  - Basic statements (Tests 4-5)"
    putStrLn $ "  - Multi-argument method calls (Tests 6-8)"
    putStrLn $ "  - Complex expressions (Tests 9-14)"
    putStrLn $ "  - Blocks and control flow (Tests 15-20)"
    putStrLn $ "  - CompoundName, modifiers, and literals (Tests 21-26)"
    putStrLn $ ""
    putStrLn $ "Quasi-Quotation Feature Demonstrated:"
    putStrLn $ "  Construction: Building AST nodes from Java syntax"
    putStrLn $ ""
    putStrLn $ "Note: Pattern matching and anti-quotation (splicing) are"
    putStrLn $ "      not currently functional for the Java grammar."
    putStrLn $ "      Only construction mode is supported."

    return 0
