import Lexer
import Parser
import Diagnostics (Diagnostic, renderDiagnostic)
import TokenProcessing
import StringLiterals
import Normalize
import GenY
import GenX
import GenQ
import DebugOptions
import qualified Debug as D
import Control.Monad (when)
import Data.Data (Data)
import Data.Maybe (catMaybes)
import Control.Exception (evaluate)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    -- Parse command-line options
    opts <- parseOptions

    -- Load grammar file
    content <- readFile (grammarFile opts)

    -- Stage 1: Lexical Analysis
    (eRawTokens, maybeT1) <- runStage opts "Lexical Analysis" $ scanTokens content
    rawTokens <- orDie opts eRawTokens

    -- Stage 1.5: Token Post-Processing
    -- Process escape sequences and concatenate multi-line strings
    (tokens, maybeT1_5) <- runStage opts "Token Post-Processing" $ processTokens rawTokens

    when (debugTokens opts) $
        D.printTokens opts tokens

    when (debugStage opts == Just StageLex)
        exitAfterDebug

    -- Stage 2: Parsing
    (eGrammar, maybeT2) <- runStage opts "Parsing" $ parse tokens
    grammar <- orDie opts eGrammar

    when (debugParse opts) $
        D.printInitialGrammar opts grammar

    when (debugStage opts == Just StageParse)
        exitAfterDebug

    -- Stage 3: String Literal Normalization
    (grammar0, maybeT3) <- runStage opts "String Normalization" $ normalizeStringLiterals grammar

    when (debugStringNorm opts) $
        D.printComparison opts "Before String Normalization" grammar "After String Normalization" grammar0

    when (debugStage opts == Just StageStringNorm)
        exitAfterDebug

    -- Stage 4: Clause Normalization
    (eGrammar1, maybeT4) <- runStage opts "Clause Normalization" $ normalizeTopLevelClauses grammar0
    grammar1 <- orDie opts eGrammar1

    when (debugClauseNorm opts) $
        D.printNormalGrammar opts "CLAUSE NORMALIZATION OUTPUT" grammar1

    when (debugStage opts == Just StageClauseNorm)
        exitAfterDebug

    -- Stage 5: Constructor Name Filling
    (grammar2, maybeT5) <- runStage opts "Constructor Name Filling" $ fillConstructorNames grammar1

    when (debugConstructors opts) $
        D.printNormalGrammar opts "FINAL GRAMMAR (with Constructor Names)" grammar2

    when (debugStage opts == Just StageFillNames)
        exitAfterDebug

    -- Statistics and Analysis (before code generation)
    when (showStats opts) $
        D.showGrammarStats opts grammar grammar2

    when (analyzeConflicts opts) $
        D.analyzeGrammarConflicts opts grammar2

    when (showRuleGraph opts) $
        D.printRuleGraph opts grammar2

    when (listRules opts) $
        D.printRuleList opts grammar2

    -- Validation
    when (validateGrammar opts) $ do
        valid <- D.runGrammarValidation opts grammar2
        when (not valid) $
            putStrLn "Warning: Grammar has validation issues."

    when (showUnusedRules opts) $
        D.findUnusedRules opts grammar2

    when (checkLeftRecursion opts) $
        D.detectLeftRecursion opts grammar2

    when (suggestShortcuts opts) $
        D.suggestGrammarShortcuts opts grammar2

    -- Expand specific rule if requested
    case expandRule opts of
        Just ruleName -> D.showExpandedRule opts grammar2 ruleName
        Nothing -> return ()

    -- Stage 6: Code Generation
    let grammar_name = getNGrammarName grammar2

    (eY, maybeT6) <- runStage opts "Parser (Y) Generation" $ genY grammar2
    y_content <- orDie opts eY

    (eX, maybeT7) <- runStage opts "Lexer (X) Generation" $ genX grammar2
    x_content <- orDie opts eX

    (eQ, maybeT8) <- runStage opts "QuasiQuoter (Q) Generation" $ genQ grammar2
    q_content <- orDie opts eQ

    -- Debug generated specs if requested
    when (debugParserSpec opts) $ do
        D.debugSection opts "GENERATED HAPPY PARSER SPECIFICATION"
        putStrLn y_content

    when (debugLexerSpec opts) $ do
        D.debugSection opts "GENERATED ALEX LEXER SPECIFICATION"
        putStrLn x_content

    when (debugQQSpec opts) $ do
        D.debugSection opts "GENERATED QUASIQUOTER CODE"
        putStrLn q_content

    -- Write output files (unless we're only validating). A spec dump still
    -- writes the files; validation alone suppresses them.
    let specDumpRequested = any id [debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts]
    when (not (validateGrammar opts) || specDumpRequested) $ do
        let dir = outputDir opts
        writeFile (dir ++ "/" ++ grammar_name ++ "Parser.y") y_content
        writeFile (dir ++ "/" ++ grammar_name ++ "Lexer.x") x_content
        writeFile (dir ++ "/" ++ grammar_name ++ "QQ.hs") q_content

    -- Show timing profile if requested
    when (profileStages opts) $ do
        let allTimings = catMaybes [maybeT1, maybeT1_5, maybeT2, maybeT3, maybeT4, maybeT5, maybeT6, maybeT7, maybeT8]
        when (not $ null allTimings) $
            D.showTimingInfo opts allTimings

    -- Success message
    when (not $ any id [debugTokens opts, debugParse opts, debugStringNorm opts,
                        debugClauseNorm opts, debugConstructors opts,
                        debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts,
                        showStats opts, validateGrammar opts]) $ do
        putStrLn $ "Successfully generated files for " ++ grammar_name

-- | Either surface a pipeline diagnostic on stderr and exit 1, or return the
-- value. The grammar file name gives the diagnostic its GNU-style prefix.
orDie :: DebugOptions -> Either Diagnostic a -> IO a
orDie opts (Left d)  = do
    hPutStrLn stderr (renderDiagnostic (grammarFile opts) d)
    exitWith (ExitFailure 1)
orDie _    (Right a) = return a

-- | Run one pure pipeline stage. Under --profile-stages the result is forced
-- to normal form inside the timed window, so the timing reflects the stage
-- that produced the value rather than the stage that first consumed it.
runStage :: Data a => DebugOptions -> String -> a -> IO (a, Maybe D.TimingInfo)
runStage opts name value
    | profileStages opts = do
        (result, timing) <- D.timed name $ evaluate $ D.deepForce value
        return (result, Just timing)
    | otherwise = return (value, Nothing)

-- Helper function
exitAfterDebug :: IO ()
exitAfterDebug = do
    putStrLn ""
    putStrLn "Stopped after requested debug stage."
    exitSuccess
