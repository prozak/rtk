import Lexer
import Parser (parse)
import Syntax
import ASTAdapter (parseWithGenerated, scanTokensGenerated)
import Diagnostics (Diagnostic (..), renderDiagnostic)
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
import Data.IORef (newIORef, readIORef, writeIORef)
import Data.Maybe (catMaybes, isJust)
import Control.Exception (IOException, evaluate, try)
import System.Directory (createDirectoryIfMissing)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitSuccess, exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
    -- Parse command-line options
    opts <- parseOptions

    -- Load grammar file
    content <- readFile (grammarFile opts)

    -- --debug-rule: trace one rule through the pipeline, printing only its
    -- representation after each stage. Remember whether it was seen at any
    -- stage so an unknown name can fail the run at the end (catches typos).
    ruleFound <- newIORef False
    let traceRule trace = case debugRule opts of
            Just name -> do
                found <- trace name
                when found $ writeIORef ruleFound True
            Nothing -> return ()

    -- Stages 1-2: front end (lexing and parsing). The default is the
    -- self-hosted front end: the lexer/parser RTK generated from
    -- test-grammars/grammar.pg (compiled from the test/golden/grammar
    -- snapshot) plus the AST adapter. --use-handwritten swaps in the
    -- hand-written reference Lexer.x/Parser.y instead; everything after this
    -- point is the same shared pipeline. The generated front end has no
    -- token post-processing stage, so --debug-tokens shows the raw generated
    -- token stream and --debug-stage lex stops after the combined front end.
    (grammar, frontEndTimings) <-
        if useGenerated opts
            then do
                when (debugTokens opts) $ do
                    D.debugSection opts "LEXER OUTPUT - TOKENS (generated front end)"
                    case scanTokensGenerated content of
                        Left _     -> return () -- the parse below reports it
                        Right toks -> do
                            putStrLn $ "Total tokens: " ++ show (length toks)
                            putStrLn ""
                            mapM_ putStrLn toks
                (eGrammar, maybeT) <- runStage opts "Front End (generated)" $
                    parseWithGenerated content
                g <- orDie opts eGrammar
                traceRule $ \name -> do
                    D.traceRuleTokensUnavailable opts name
                    return False
                when (debugStage opts == Just StageLex)
                    exitAfterDebug
                return (g, [maybeT])
            else do
                -- Stage 1: Lexical Analysis
                (eRawTokens, maybeT1) <- runStage opts "Lexical Analysis" $ scanTokens content
                rawTokens <- orDie opts eRawTokens

                -- Stage 1.5: Token Post-Processing
                -- Process escape sequences and concatenate multi-line strings
                (tokens, maybeT1_5) <- runStage opts "Token Post-Processing" $ processTokens rawTokens

                when (debugTokens opts) $
                    D.printTokens opts tokens

                traceRule $ \name -> D.traceRuleTokens opts name tokens

                when (debugStage opts == Just StageLex)
                    exitAfterDebug

                -- Stage 2: Parsing
                (eGrammar, maybeT2) <- runStage opts "Parsing" $ parse tokens
                g <- orDie opts eGrammar
                return (g, [maybeT1, maybeT1_5, maybeT2])

    when (debugParse opts) $
        D.printInitialGrammar opts grammar

    traceRule $ \name -> D.traceRuleInitial opts name "After Parse" grammar

    when (debugStage opts == Just StageParse)
        exitAfterDebug

    -- Stage 3: String Literal Normalization
    (grammar0, maybeT3) <- runStage opts "String Normalization" $ normalizeStringLiterals grammar

    when (debugStringNorm opts) $
        D.printComparison opts "Before String Normalization" grammar "After String Normalization" grammar0

    traceRule $ \name -> D.traceRuleInitial opts name "After String Normalization" grammar0

    when (debugStage opts == Just StageStringNorm)
        exitAfterDebug

    -- Stage 4: Clause Normalization
    (eGrammar1, maybeT4) <- runStage opts "Clause Normalization" $ normalizeTopLevelClauses grammar0
    grammar1 <- orDie opts eGrammar1

    when (debugClauseNorm opts) $
        D.printNormalGrammar opts "CLAUSE NORMALIZATION OUTPUT" grammar1

    traceRule $ \name -> D.traceRuleNormal opts name "After Clause Normalization" grammar1

    when (debugStage opts == Just StageClauseNorm)
        exitAfterDebug

    -- Stage 5: Constructor Name Filling
    (grammar2, maybeT5) <- runStage opts "Constructor Name Filling" $ fillConstructorNames grammar1

    when (debugConstructors opts) $
        D.printNormalGrammar opts "FINAL GRAMMAR (with Constructor Names)" grammar2

    traceRule $ \name -> D.traceRuleNormal opts name "After Constructor Fill" grammar2

    -- Constructor fill is the last traced stage: a rule that matched nowhere
    -- can only be a typo, so fail the run
    case debugRule opts of
        Just name -> do
            found <- readIORef ruleFound
            when (not found) $ do
                hPutStrLn stderr $
                    "rtk: rule '" ++ name ++ "' was not found at any pipeline stage"
                exitWith (ExitFailure 1)
        Nothing -> return ()

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
        -- A missing output directory is created on the fly; any remaining
        -- IO failure (no permission, a file where the directory should be)
        -- is rendered as a one-line diagnostic instead of escaping as an
        -- uncaught IOException
        written <- try $ do
            createDirectoryIfMissing True dir
            writeFile (dir ++ "/" ++ grammar_name ++ "Parser.y") y_content
            writeFile (dir ++ "/" ++ grammar_name ++ "Lexer.x") x_content
            writeFile (dir ++ "/" ++ grammar_name ++ "QQ.hs") q_content
        case written of
            Left e -> orDie opts $ Left $ Diagnostic Nothing Nothing $
                "cannot write generated files: " ++ show (e :: IOException)
            Right () -> return ()

    -- Show timing profile if requested
    when (profileStages opts) $ do
        let allTimings = catMaybes (frontEndTimings ++ [maybeT3, maybeT4, maybeT5, maybeT6, maybeT7, maybeT8])
        when (not $ null allTimings) $
            D.showTimingInfo opts allTimings

    -- Success message
    when (not $ any id [debugTokens opts, debugParse opts, debugStringNorm opts,
                        debugClauseNorm opts, debugConstructors opts,
                        debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts,
                        showStats opts, validateGrammar opts,
                        isJust (debugRule opts)]) $ do
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
