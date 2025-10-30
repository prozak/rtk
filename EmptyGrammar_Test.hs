module Main where

import Lexer (alexScanTokens)
import Parser (parse)
import StringLiterals (normalizeStringLiterals)
import Normalize (normalizeTopLevelClauses)
import Test.HUnit
import Control.Exception (catch, evaluate, ErrorCall(..))

-- Test that an empty grammar file (with grammar declaration but no rules)
-- produces the expected error message
testEmptyGrammar :: Test
testEmptyGrammar = TestCase $ do
    let content = "grammar 'Empty';"
    let grammar = parse . alexScanTokens $ content
    let grammar0 = normalizeStringLiterals grammar

    -- Attempt to normalize the grammar, which should fail with our expected message
    result <- catch
        (do
            _ <- evaluate $ normalizeTopLevelClauses grammar0
            return (Left "No error thrown")
        )
        (\(ErrorCall msg) -> return (Right msg))

    case result of
        Left err -> assertFailure $ "Expected error about empty grammar, but got: " ++ err
        Right msg -> do
            assertBool
                ("Error message should mention empty grammar. Got: " ++ msg)
                ("contains no rules" `isInfixOf` msg && "Empty" `isInfixOf` msg)
            putStrLn $ "✓ Empty grammar test passed: " ++ msg

-- Test that a completely empty file produces a parse error
testCompletelyEmptyFile :: Test
testCompletelyEmptyFile = TestCase $ do
    let content = ""

    result <- catch
        (do
            _ <- evaluate $ parse . alexScanTokens $ content
            return (Left "No error thrown")
        )
        (\(ErrorCall msg) -> return (Right msg))

    case result of
        Left err -> assertFailure $ "Expected parse error for empty file, but got: " ++ err
        Right msg -> do
            assertBool
                ("Error message should mention parse error. Got: " ++ msg)
                ("Parse error" `isInfixOf` msg)
            putStrLn $ "✓ Empty file test passed: " ++ msg

-- Test that a valid grammar with rules works correctly
testValidGrammar :: Test
testValidGrammar = TestCase $ do
    let content = "grammar 'Valid';\nrule = 'test' ;"
    let grammar = parse . alexScanTokens $ content
    let grammar0 = normalizeStringLiterals grammar

    result <- catch
        (do
            _ <- evaluate $ normalizeTopLevelClauses grammar0
            return (Right ())
        )
        (\(ErrorCall msg) -> return (Left msg))

    case result of
        Left err -> assertFailure $ "Valid grammar should parse without error, but got: " ++ err
        Right _ -> putStrLn "✓ Valid grammar test passed"

-- Helper function to check if a string contains a substring
isInfixOf :: String -> String -> Bool
isInfixOf needle haystack = any (isPrefixOf needle) (tails haystack)
  where
    tails [] = [[]]
    tails s@(_:xs) = s : tails xs

    isPrefixOf [] _ = True
    isPrefixOf _ [] = False
    isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

main :: IO ()
main = do
    putStrLn "\n=== Running Empty Grammar Tests ==="
    putStrLn ""
    counts <- runTestTT $ TestList
        [ TestLabel "Empty grammar with declaration" testEmptyGrammar
        , TestLabel "Completely empty file" testCompletelyEmptyFile
        , TestLabel "Valid grammar with rules" testValidGrammar
        ]
    putStrLn ""
    if errors counts + failures counts == 0
        then putStrLn "=== All tests PASSED ==="
        else putStrLn "=== Some tests FAILED ==="
    return ()
