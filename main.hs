import System.Environment(getArgs)
import Lexer
import Parser
import StringLiterals
import Normalize
import GenY
import GenX
import GenQ
import DebugOptions
import qualified Debug as D
import System.IO (hFlush, stdout)
import Control.Monad (when)

main :: IO ()
main = do
    -- Parse command-line options
    opts <- parseOptions

    -- Load grammar file
    content <- readFile (grammarFile opts)

    -- Initialize timing information list
    timings <- if profileStages opts
                  then return []
                  else return []

    -- Stage 1: Lexical Analysis
    (tokens, timings1) <- if profileStages opts
        then D.timed "Lexical Analysis" $ evaluate' $ alexScanTokens content
        else return (alexScanTokens content, [])

    when (debugTokens opts) $
        D.debugTokens opts tokens

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageLex) $
        exitAfterDebug

    -- Stage 2: Parsing
    (grammar, timings2) <- if profileStages opts
        then D.timed "Parsing" $ evaluate' $ parse tokens
        else return (parse tokens, [])

    when (debugParse opts) $
        D.debugInitialGrammar opts grammar

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageParse) $
        exitAfterDebug

    -- Stage 3: String Literal Normalization
    (grammar0, timings3) <- if profileStages opts
        then D.timed "String Normalization" $ evaluate' $ normalizeStringLiterals grammar
        else return (normalizeStringLiterals grammar, [])

    when (debugStringNorm opts) $
        D.debugComparison opts "Before String Normalization" grammar "After String Normalization" grammar0

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageStringNorm) $
        exitAfterDebug

    -- Stage 4: Clause Normalization
    (grammar1, timings4) <- if profileStages opts
        then D.timed "Clause Normalization" $ evaluate' $ normalizeTopLevelClauses grammar0
        else return (normalizeTopLevelClauses grammar0, [])

    when (debugClauseNorm opts) $
        D.debugNormalGrammar opts "CLAUSE NORMALIZATION OUTPUT" grammar1

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageClauseNorm) $
        exitAfterDebug

    -- Stage 5: Constructor Name Filling
    (grammar2, timings5) <- if profileStages opts
        then D.timed "Constructor Name Filling" $ evaluate' $ fillConstructorNames grammar1
        else return (fillConstructorNames grammar1, [])

    when (debugConstructors opts) $
        D.debugNormalGrammar opts "FINAL GRAMMAR (with Constructor Names)" grammar2

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageFillNames) $
        exitAfterDebug

    -- Statistics and Analysis (before code generation)
    when (showStats opts) $
        D.showGrammarStats opts grammar grammar2

    when (analyzeConflicts opts) $
        D.analyzeGrammarConflicts opts grammar2

    when (showRuleGraph opts) $
        D.showRuleGraph opts grammar2

    when (listRules opts) $
        D.showRuleList opts grammar2

    -- Validation
    when (validateGrammar opts) $ do
        valid <- D.validateGrammar opts grammar2
        when (not valid) $
            putStrLn "Warning: Grammar has validation issues."

    when (showUnusedRules opts) $
        D.findUnusedRules opts grammar2

    when (checkLeftRecursion opts) $
        D.checkLeftRecursion opts grammar2

    when (suggestShortcuts opts) $
        D.suggestShortcuts opts grammar2

    -- Expand specific rule if requested
    case expandRule opts of
        Just ruleName -> D.expandRule opts grammar2 ruleName
        Nothing -> return ()

    -- Stage 6: Code Generation
    let grammar_name = getNGrammarName grammar2

    (y_content, timings6) <- if profileStages opts
        then D.timed "Y Code Generation" $ evaluate' $ genY grammar2
        else return (genY grammar2, [])

    (x_content, timings7) <- if profileStages opts
        then D.timed "X Code Generation" $ evaluate' $ genX grammar2
        else return (genX grammar2, [])

    (q_content, timings8) <- if profileStages opts
        then D.timed "Q Code Generation" $ evaluate' $ genQ grammar2
        else return (genQ grammar2, [])

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

    -- Write output files (unless we're only validating)
    when (not (validateGrammar opts) || not (validateGrammar opts && not (any id [debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts]))) $ do
        let dir = outputDir opts
        writeFile (dir ++ "/" ++ grammar_name ++ "Parser.y") y_content
        writeFile (dir ++ "/" ++ grammar_name ++ "Lexer.x") x_content
        writeFile (dir ++ "/" ++ grammar_name ++ "QQ.hs") q_content

    -- Show timing profile if requested
    when (profileStages opts) $ do
        let allTimings = concat [timings1, timings2, timings3, timings4, timings5, timings6, timings7, timings8]
        when (not $ null allTimings) $
            D.showTimingInfo opts allTimings

    -- Success message
    when (not $ any id [debugTokens opts, debugParse opts, debugStringNorm opts,
                        debugClauseNorm opts, debugConstructors opts,
                        debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts,
                        showStats opts, validateGrammar opts]) $ do
        putStrLn $ "Successfully generated files for " ++ grammar_name

-- Helper functions
evaluate' :: a -> IO a
evaluate' x = return x

exitAfterDebug :: IO ()
exitAfterDebug = do
    putStrLn ""
    putStrLn "Stopped after requested debug stage."
    error "Debug stage exit"

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

fromJust :: Maybe a -> a
fromJust (Just x) = x
fromJust Nothing = error "fromJust: Nothing"
