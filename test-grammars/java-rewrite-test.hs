{-# LANGUAGE QuasiQuotes #-}

-- The user-facing rewrite recipe of task 8d, exercised end to end
-- (`make test-java-rewrite`; the worked walkthrough is the "Rewriting
-- parsed Java" section of docs/java-quasi-quotation-tests.md):
-- a rewrite over a generated AST is an ordinary function whose match arms
-- are quasi-quoted patterns and whose results are quasi-quoted expressions,
-- applied to every node of any AST value with SYB's `everywhere`, and the
-- same patterns drive queries through `everything`. rtk's own pipeline
-- (Frontend/StringLiterals/Normalize) and the write-you-a-haskell
-- tutorial's evaluator use exactly this shape.

import qualified JavaQQ as J
import qualified JavaParser as JP
import Data.Generics (everywhere, everything, mkT, mkQ)
import Text.Show.Pretty (ppShow)

-- Comparisons against null become Yoda-style (null first): one arm per
-- shape, Java syntax on both sides, everything else passes through.
yoda :: JP.Expression -> JP.Expression
yoda [J.expression| $Expression:x == null |] = [J.expression| null == $Expression:x |]
yoda [J.expression| $Expression:x != null |] = [J.expression| null != $Expression:x |]
yoda e = e

-- The same patterns as a query: count the comparisons yoda would rewrite.
pendingNullChecks :: JP.Expression -> Int
pendingNullChecks [J.expression| $Expression:_x == null |] = 1
pendingNullChecks [J.expression| $Expression:_x != null |] = 1
pendingNullChecks _ = 0

main :: IO ()
main = do
    putStrLn "=========================================================="
    putStrLn "Java Rewrite Recipe Test (QQ patterns + SYB)"
    putStrLn "=========================================================="
    putStrLn ""

    -- `everywhere (mkT yoda)` rewrites every expression inside the block -
    -- the if condition and the while condition here - in one pass; the
    -- expected result is written in Java too, so the whole test is two
    -- quotes and a traversal. Positions are equality-transparent, which is
    -- why ASTs from different quotes compare equal structurally.
    let body = [J.statementBlock| {
            if (name == null) { return defaultName; }
            while (cursor != null) { cursor = cursor.next(); }
            return name;
        } |]
        expected = [J.statementBlock| {
            if (null == name) { return defaultName; }
            while (null != cursor) { cursor = cursor.next(); }
            return name;
        } |]
        rewritten = everywhere (mkT yoda) body

    check "comparisons at every statement depth become Yoda-style"
          (rewritten == expected)
          (ppShow rewritten)

    -- The query runs over the same block: two rewritable comparisons
    -- before the pass, none after (the rewritten form no longer matches).
    check "the query counts both pending comparisons"
          (everything (+) (0 `mkQ` pendingNullChecks) body == 2)
          (show (everything (+) (0 `mkQ` pendingNullChecks) body))
    check "after the rewrite no pending comparison remains"
          (everything (+) (0 `mkQ` pendingNullChecks) rewritten == 0)
          (show (everything (+) (0 `mkQ` pendingNullChecks) rewritten))

    -- The honest boundary (see the docs section): a pattern metavariable
    -- sits at the splice attach point of the expression chain, so it
    -- matches comparands that parse at that chain position (identifiers,
    -- literals, calls, parenthesized expressions) - a binary comparand
    -- lives higher in the chain and passes through unmatched.
    let call = [J.expression| getName() == null |]
        sums = [J.expression| a + b == null |]
    check "a method-call comparand is matched and rewritten"
          (everywhere (mkT yoda) call == [J.expression| null == getName() |])
          (ppShow (everywhere (mkT yoda) call))
    check "a binary comparand is beyond the pattern and passes through"
          (everywhere (mkT yoda) sums == sums)
          (ppShow (everywhere (mkT yoda) sums))

    putStrLn ""
    putStrLn "=========================================================="
    putStrLn "All rewrite recipe tests passed!"
    putStrLn "=========================================================="

-- A wrong result must fail the test binary (and `make test-java-rewrite`).
check :: String -> Bool -> String -> IO ()
check what ok got
    | ok        = putStrLn $ "OK " ++ what
    | otherwise = error $ "FAILED: " ++ what ++ ", got:\n" ++ got
