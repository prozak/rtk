-- | Golden tests for the code generators.
--
-- For every grammar under test-grammars/ the full in-process pipeline is run
-- and the generated <Name>Lexer.x, <Name>Parser.y and <Name>QQ.hs are compared
-- byte-for-byte against snapshots checked in under test/golden/<grammar>/.
-- This catches generator regressions instantly, without alex, happy or a GHC
-- compile cycle.
--
-- Every grammar is run through BOTH front ends — the self-hosted one that
-- RTK generated from grammar.pg (the default) and the hand-written reference
-- lexer/parser (--use-handwritten) — and both must reproduce the same
-- snapshots byte-for-byte. This is the self-hosting equivalence harness:
-- source positions only affect diagnostics, never the generated artifacts,
-- so any artifact divergence on a valid grammar is a real front-end bug.
-- Grammars pinned in TestSupport.frontEndDivergentGrammars (none today)
-- would be checked with the reference front end only, plus a guard that
-- fails once they stop diverging.
--
-- After an intentional generator change, refresh the snapshots with
-- `make accept-golden` (or `RTK_ACCEPT=1 cabal test golden`) and review the
-- resulting diff like any other code change.
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (forM, forM_, unless, when)
import Data.List (isInfixOf, sort)
import Data.Maybe (listToMaybe)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist,
                         doesFileExist, listDirectory)
import System.Environment (getArgs, lookupEnv)
import System.Exit (exitFailure)
import System.FilePath ((</>), takeBaseName)
import Test.HUnit

import qualified GrammarParser as GP

import Diagnostics (Diagnostic, renderDiagnostic)
import TestSupport

goldenRoot :: FilePath
goldenRoot = "test" </> "golden"

-- | Grammars that are known not to generate, with a fragment of the expected
-- error. Currently empty: every grammar in test-grammars/ generates. Pin a
-- grammar here only while a known generation defect is being worked on; the
-- test fails once the grammar generates again so the entry gets dropped.
knownBrokenGrammars :: [(String, String)]
knownBrokenGrammars = []

regenerateHint :: String
regenerateHint =
    "\nIf the new output is intentional, refresh the snapshots with" ++
    "\n  make accept-golden" ++
    "\nand review the diff of test/golden/ before committing."

-- | Run the pipeline on one grammar file, forcing the generated contents so
-- that any lazy `error` from the generators surfaces here as a test failure.
generateArtifacts :: (String -> Either Diagnostic GP.Grammar)
                  -> FilePath -> IO (Either String [(FilePath, String)])
generateArtifacts parseSrc pgFile = do
    source <- readFileUtf8 pgFile
    result <- try $
        case parseSrc source >>= normalizeParsedGrammar >>= artifactsFor of
            Left d          -> return (Left (renderDiagnostic pgFile d))
            Right artifacts -> do
                mapM_ (\(_, content) -> evaluate (length content)) artifacts
                return (Right artifacts)
    return $ case result of
        Left e      -> Left (show (e :: SomeException))
        Right inner -> inner

-- | A short, readable summary of where two artifacts diverge.
diffSummary :: String -> String -> String
diffSummary expected actual =
    unlines $ sizes : maybe onlyNewlines describe firstDiff
  where
    el = lines expected
    al = lines actual
    sizes = "golden has " ++ show (length el) ++ " lines, generated output has "
            ++ show (length al) ++ " lines"
    n = max (length el) (length al)
    pad xs = take n (xs ++ repeat "<no such line>")
    firstDiff = listToMaybe [ d | d@(_, e, a) <- zip3 [1 :: Int ..] (pad el) (pad al)
                                , e /= a ]
    describe (lineNo, e, a) =
        [ "first difference at line " ++ show lineNo
        , "  golden:    " ++ e
        , "  generated: " ++ a
        ]
    onlyNewlines = ["contents differ only in trailing whitespace/newlines"]

goldenTestFor :: FilePath -> IO Test
goldenTestFor pgFile = do
    let grammarKey = takeBaseName pgFile
    handResult <- generateArtifacts parseGrammarSource pgFile
    genResult  <- generateArtifacts parseGrammarSourceGenerated pgFile
    genTest <- case lookup grammarKey frontEndDivergentGrammars of
        Nothing     -> return $ frontEndTest grammarKey genResult
        Just reason -> divergenceGuard grammarKey reason genResult
    return $ TestLabel grammarKey $ TestList
        [ TestLabel "hand-written front end" (frontEndTest grammarKey handResult)
        , TestLabel "generated front end" genTest
        ]
  where
    frontEndTest grammarKey generated = case lookup grammarKey knownBrokenGrammars of
        Just expectedError -> TestCase $ case generated of
            Left err -> assertBool
                ("expected failure mentioning " ++ show expectedError ++ ", got:\n" ++ err)
                (expectedError `isInfixOf` err)
            Right _ -> assertFailure $
                pgFile ++ " generates now; drop it from knownBrokenGrammars and run"
                ++ " `make accept-golden` to snapshot it"
        Nothing -> case generated of
            Left err -> TestCase $ assertFailure $
                "generation failed for " ++ pgFile ++ ":\n" ++ err
            Right artifacts -> TestList (map (check grammarKey) artifacts)

    check grammarKey (fileName, actual) = TestLabel fileName $ TestCase $ do
        let goldenFile = goldenRoot </> grammarKey </> fileName
        exists <- doesFileExist goldenFile
        if not exists
            then assertFailure $ "missing golden file " ++ goldenFile ++ regenerateHint
            else do
                expected <- readFileUtf8 goldenFile
                when (expected /= actual) $ assertFailure $
                    "generated output differs from " ++ goldenFile ++ ":\n"
                    ++ diffSummary expected actual ++ regenerateHint

    -- A pinned divergent grammar must actually diverge: as soon as the
    -- generated front end reproduces the snapshots too, this fails so the
    -- pin gets dropped and the grammar joins the strict equivalence check.
    divergenceGuard grammarKey reason generated =
        return $ TestLabel "known divergence (pinned in TestSupport)" $ TestCase $
            case generated of
                Left _ -> return () -- rejected by the generated front end
                Right artifacts -> do
                    same <- mapM (matchesGolden grammarKey) artifacts
                    when (and same) $ assertFailure $
                        "the generated front end now reproduces the snapshots for this grammar\n"
                        ++ "(pinned because: " ++ reason ++ ");\n"
                        ++ "drop it from frontEndDivergentGrammars in test/TestSupport.hs"

    matchesGolden grammarKey (fileName, actual) = do
        let goldenFile = goldenRoot </> grammarKey </> fileName
        exists <- doesFileExist goldenFile
        if exists
            then (== actual) <$> readFileUtf8 goldenFile
            else return False

-- | Guard against stale snapshots: every directory under test/golden/ must
-- correspond to an existing grammar file.
staleGoldenTest :: [FilePath] -> Test
staleGoldenTest pgFiles = TestLabel "no stale golden directories" $ TestCase $ do
    hasGolden <- doesDirectoryExist goldenRoot
    entries <- if hasGolden then listDirectory goldenRoot else return []
    let known = map takeBaseName pgFiles
        stale = [ e | e <- sort entries, e `notElem` known ]
    unless (null stale) $ assertFailure $
        "golden directories without a matching grammar in " ++ grammarsDir ++ ": "
        ++ show stale ++ "\nDelete them (or restore the grammar)."

acceptAll :: [FilePath] -> IO ()
acceptAll pgFiles = do
    failed <- forM pgFiles $ \pgFile -> do
        let grammarKey = takeBaseName pgFile
        if grammarKey `elem` map fst knownBrokenGrammars
            then do
                putStrLn $ "skipping " ++ pgFile ++ " (listed in knownBrokenGrammars)"
                return False
            else do
                -- Snapshots are produced by the generated (default) front
                -- end — grammar.pg is the authority; the hand-written
                -- reference front end must then reproduce them. A grammar
                -- pinned as divergent (none today) is snapshotted from the
                -- reference front end, matching how it is checked.
                let parseSrc = case lookup grammarKey frontEndDivergentGrammars of
                        Just _  -> parseGrammarSource
                        Nothing -> parseGrammarSourceGenerated
                generated <- generateArtifacts parseSrc pgFile
                case generated of
                    Left err -> do
                        putStrLn $ "FAILED to generate " ++ pgFile ++ ":\n" ++ err
                        return True
                    Right artifacts -> do
                        let dir = goldenRoot </> grammarKey
                        createDirectoryIfMissing True dir
                        forM_ artifacts $ \(fileName, content) -> do
                            writeFileUtf8 (dir </> fileName) content
                            putStrLn $ "wrote " ++ (dir </> fileName)
                        return False
    when (or failed) exitFailure

main :: IO ()
main = do
    args <- getArgs
    acceptEnv <- lookupEnv "RTK_ACCEPT"
    let accept = "--accept" `elem` args || maybe False (not . null) acceptEnv
    pgFiles <- discoverGrammarFiles grammarsDir
    when (null pgFiles) $ do
        putStrLn $ "no grammar files found under " ++ grammarsDir
        exitFailure
    if accept
        then acceptAll pgFiles
        else do
            perGrammar <- mapM goldenTestFor pgFiles
            results <- runTestTT $ TestList (staleGoldenTest pgFiles : perGrammar)
            when (errors results + failures results /= 0) exitFailure
