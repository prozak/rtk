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

    putStrLn "\n=== All QuasiQuoter tests completed successfully! ==="
    putStrLn "\nNext steps for debugging:"
    putStrLn "- Try multi-argument method calls"
    putStrLn "- Test more complex expressions"
    putStrLn "- Test pattern matching with quasi-quotations"
    putStrLn "- Test blocks and method declarations"

    return 0
