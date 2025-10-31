{-# LANGUAGE QuasiQuotes #-}

import System.IO(readFile, writeFile)
import System.Environment(getArgs)
import JavaLexer
import JavaParser
import JavaQQ
import Text.Show.Pretty

-- Test pattern matching with correctly named variables
-- Variable names must start with a prefix that matches a rule name
main = do
    putStrLn "=== Java QuasiQuoter Pattern Matching Tests ==="

    -- Test 1: Pattern matching with expression rule
    -- Use variables like $expression1, $expr1, $e1 (prefix matches "Expression" or "E")
    putStrLn "\n--- Test 1: Binary Addition Pattern ---"
    let testExpr = [expression| x + y |]
    case testExpr of
        [expression| $expression1 + $expression2 |] -> do
            putStrLn "Matched binary addition!"
            putStrLn $ "  Left: " ++ ppShow expression1
            putStrLn $ "  Right: " ++ ppShow expression2
        _ -> putStrLn "No match"

    putStrLn "\n=== Pattern matching test completed! ==="
    return 0
