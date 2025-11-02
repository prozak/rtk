import Lexer
import Parser
import TokenProcessing
import StringLiterals
import Normalize
import GenY
import GenX
import GenQ
import DebugOptions
import qualified Debug as D
import Control.Monad (when)
import Data.Maybe (isJust, fromJust, catMaybes)
import Control.Exception (evaluate)

main :: IO ()
main = do
    -- Parse command-line options
    opts <- parseOptions

    -- Check for experimental generated parser mode
    when (useGenerated opts) $ do
        putStrLn "=========================================="
        putStrLn "EXPERIMENTAL: Using Generated Parsers"
        putStrLn "=========================================="
        putStrLn ""
        putStrLn "This mode uses parsers generated from test-grammars/grammar.pg"
        putStrLn "instead of the hand-written Lexer.x and Parser.y."
        putStrLn ""
        putStrLn "Status: NOT YET IMPLEMENTED"
        putStrLn ""
        putStrLn "To implement:"
        putStrLn "  1. Run: make test-grammar"
        putStrLn "  2. This generates: test-out/GrammarLexer.x, GrammarParser.y, GrammarQQ.hs"
        putStrLn "  3. Compile these to create generated parser modules"
        putStrLn "  4. Integrate into main.hs dual-mode logic"
        putStrLn ""
        putStrLn "This is Prototype 1 of the self-hosting roadmap."
        putStrLn "See docs/self-hosting-strategy.md for details."
        putStrLn "=========================================="
        error "Generated parser mode not yet available"

    -- Load grammar file
    content <- readFile (grammarFile opts)

    -- Stage 1: Lexical Analysis
    (rawTokens, maybeT1) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Lexical Analysis" $ evaluate $ alexScanTokens content
            return (result, Just timing)
        else return (alexScanTokens content, Nothing)

    -- Stage 1.5: Token Post-Processing
    -- Process escape sequences and concatenate multi-line strings
    (tokens, maybeT1_5) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Token Post-Processing" $ evaluate $ processTokens rawTokens
            return (result, Just timing)
        else return (processTokens rawTokens, Nothing)

    when (debugTokens opts) $
        D.printTokens opts tokens

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageLex) $
        exitAfterDebug

    -- Stage 2: Parsing
    (grammar, maybeT2) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Parsing" $ evaluate $ parse tokens
            return (result, Just timing)
        else return (parse tokens, Nothing)

    when (debugParse opts) $
        D.printInitialGrammar opts grammar

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageParse) $
        exitAfterDebug

    -- Stage 3: String Literal Normalization
    (grammar0, maybeT3) <- if profileStages opts
        then do
            (result, timing) <- D.timed "String Normalization" $ evaluate $ normalizeStringLiterals grammar
            return (result, Just timing)
        else return (normalizeStringLiterals grammar, Nothing)

    when (debugStringNorm opts) $
        D.printComparison opts "Before String Normalization" grammar "After String Normalization" grammar0

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageStringNorm) $
        exitAfterDebug

    -- Stage 4: Clause Normalization
    (grammar1, maybeT4) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Clause Normalization" $ evaluate $ normalizeTopLevelClauses grammar0
            return (result, Just timing)
        else return (normalizeTopLevelClauses grammar0, Nothing)

    when (debugClauseNorm opts) $
        D.printNormalGrammar opts "CLAUSE NORMALIZATION OUTPUT" grammar1

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageClauseNorm) $
        exitAfterDebug

    -- Stage 5: Constructor Name Filling
    (grammar2, maybeT5) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Constructor Name Filling" $ evaluate $ fillConstructorNames grammar1
            return (result, Just timing)
        else return (fillConstructorNames grammar1, Nothing)

    when (debugConstructors opts) $
        D.printNormalGrammar opts "FINAL GRAMMAR (with Constructor Names)" grammar2

    when (isJust (debugStage opts) && fromJust (debugStage opts) == StageFillNames) $
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

    (y_content, maybeT6) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Parser (Y) Generation" $ evaluate $ genY grammar2
            return (result, Just timing)
        else return (genY grammar2, Nothing)

    (x_content, maybeT7) <- if profileStages opts
        then do
            (result, timing) <- D.timed "Lexer (X) Generation" $ evaluate $ genX grammar2
            return (result, Just timing)
        else return (genX grammar2, Nothing)

    (q_content, maybeT8) <- if profileStages opts
        then do
            (result, timing) <- D.timed "QuasiQuoter (Q) Generation" $ evaluate $ genQ grammar2
            return (result, Just timing)
        else return (genQ grammar2, Nothing)

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
        let allTimings = catMaybes [maybeT1, maybeT1_5, maybeT2, maybeT3, maybeT4, maybeT5, maybeT6, maybeT7, maybeT8]
        when (not $ null allTimings) $
            D.showTimingInfo opts allTimings

    -- Success message
    when (not $ any id [debugTokens opts, debugParse opts, debugStringNorm opts,
                        debugClauseNorm opts, debugConstructors opts,
                        debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts,
                        showStats opts, validateGrammar opts]) $ do
        putStrLn $ "Successfully generated files for " ++ grammar_name

-- Helper function
exitAfterDebug :: IO ()
exitAfterDebug = do
    putStrLn ""
    putStrLn "Stopped after requested debug stage."
    error "Debug stage exit"
