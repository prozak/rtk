module DebugOptions
    ( DebugOptions(..)
    , DebugFormat(..)
    , DebugStage(..)
    , parseOptions
    , defaultDebugOptions
    ) where

import Options.Applicative

-- | Output format for debug information
data DebugFormat = FormatPretty
                 | FormatCompact
                 | FormatJSON
                 | FormatTree
                 deriving (Eq, Show)

-- | Compilation stages for selective debugging
data DebugStage = StageLex
                | StageParse
                | StageStringNorm
                | StageClauseNorm
                | StageFillNames
                | StageGen
                deriving (Eq, Show, Enum, Bounded)

-- | All debugging options
data DebugOptions = DebugOptions
    { -- Required arguments
      grammarFile :: FilePath
    , outputDir :: FilePath

    -- Pipeline stage inspection
    , debugTokens :: Bool
    , debugParse :: Bool
    , debugStringNorm :: Bool
    , debugClauseNorm :: Bool
    , debugConstructors :: Bool

    -- Output stage inspection
    , debugParserSpec :: Bool
    , debugLexerSpec :: Bool
    , debugQQSpec :: Bool

    -- Analysis and statistics
    , showStats :: Bool
    , analyzeConflicts :: Bool
    , showRuleGraph :: Bool
    , listRules :: Bool

    -- Selective debug
    , debugRule :: Maybe String
    , debugStage :: Maybe DebugStage

    -- Comparison and validation
    , compareStages :: Bool
    , validateGrammar :: Bool
    , showUnusedRules :: Bool
    , checkLeftRecursion :: Bool
    , suggestShortcuts :: Bool
    , expandRule :: Maybe String

    -- Output format
    , debugFormat :: DebugFormat
    , debugColor :: Bool

    -- Performance profiling
    , profileStages :: Bool
    , memoryStats :: Bool

    -- Export and logging
    , debugOutputDir :: Maybe FilePath
    , debugLog :: Maybe FilePath

    -- Special modes
    , interactive :: Bool
    , useGenerated :: Bool  -- Use generated parsers instead of hand-written
    } deriving (Eq, Show)

-- | Default debug options (all disabled)
defaultDebugOptions :: FilePath -> FilePath -> DebugOptions
defaultDebugOptions file dir = DebugOptions
    { grammarFile = file
    , outputDir = dir
    , debugTokens = False
    , debugParse = False
    , debugStringNorm = False
    , debugClauseNorm = False
    , debugConstructors = False
    , debugParserSpec = False
    , debugLexerSpec = False
    , debugQQSpec = False
    , showStats = False
    , analyzeConflicts = False
    , showRuleGraph = False
    , listRules = False
    , debugRule = Nothing
    , debugStage = Nothing
    , compareStages = False
    , validateGrammar = False
    , showUnusedRules = False
    , checkLeftRecursion = False
    , suggestShortcuts = False
    , expandRule = Nothing
    , debugFormat = FormatPretty
    , debugColor = True
    , profileStages = False
    , memoryStats = False
    , debugOutputDir = Nothing
    , debugLog = Nothing
    , interactive = False
    , useGenerated = False  -- Default to hand-written parsers
    }

-- | Parse debug stage from string
parseStage :: String -> Maybe DebugStage
parseStage "lex" = Just StageLex
parseStage "parse" = Just StageParse
parseStage "string-norm" = Just StageStringNorm
parseStage "clause-norm" = Just StageClauseNorm
parseStage "fill-names" = Just StageFillNames
parseStage "gen" = Just StageGen
parseStage _ = Nothing

-- | Parse debug format from string
parseFormat :: String -> Maybe DebugFormat
parseFormat "pretty" = Just FormatPretty
parseFormat "compact" = Just FormatCompact
parseFormat "json" = Just FormatJSON
parseFormat "tree" = Just FormatTree
parseFormat _ = Nothing

-- | Command-line parser for debug options
debugOptionsParser :: Parser DebugOptions
debugOptionsParser = DebugOptions
    <$> argument str
        ( metavar "GRAMMAR_FILE"
       <> help "Input grammar file" )
    <*> argument str
        ( metavar "OUTPUT_DIR"
       <> help "Output directory for generated files" )

    -- Pipeline stage inspection
    <*> switch
        ( long "debug-tokens"
       <> short 't'
       <> help "Print tokens after lexical analysis" )
    <*> switch
        ( long "debug-parse"
       <> short 'p'
       <> help "Print grammar after parsing" )
    <*> switch
        ( long "debug-string-norm"
       <> help "Print grammar before and after string normalization" )
    <*> switch
        ( long "debug-clause-norm"
       <> help "Print grammar after clause normalization" )
    <*> switch
        ( long "debug-constructors"
       <> short 'c'
       <> help "Print grammar after constructor name generation" )

    -- Output stage inspection
    <*> switch
        ( long "debug-parser-spec"
       <> help "Print generated Happy parser specification" )
    <*> switch
        ( long "debug-lexer-spec"
       <> help "Print generated Alex lexer specification" )
    <*> switch
        ( long "debug-qq-spec"
       <> help "Print generated quasiquoter code" )

    -- Analysis and statistics
    <*> switch
        ( long "stats"
       <> short 's'
       <> help "Print compilation statistics" )
    <*> switch
        ( long "analyze-conflicts"
       <> help "Analyze potential parser conflicts" )
    <*> switch
        ( long "show-rule-graph"
       <> help "Show dependency graph between rules" )
    <*> switch
        ( long "list-rules"
       <> help "List all rule names by category" )

    -- Selective debug
    <*> optional (strOption
        ( long "debug-rule"
       <> metavar "RULENAME"
       <> help "Debug a specific rule through all stages" ))
    <*> optional (option (maybeReader parseStage)
        ( long "debug-stage"
       <> metavar "STAGE"
       <> help "Stop after a specific stage (lex|parse|string-norm|clause-norm|fill-names|gen)" ))

    -- Comparison and validation
    <*> switch
        ( long "compare-stages"
       <> help "Show differences between consecutive transformation stages" )
    <*> switch
        ( long "validate-grammar"
       <> help "Run validation checks without generating output" )
    <*> switch
        ( long "unused-rules"
       <> help "Find rules that are defined but never referenced" )
    <*> switch
        ( long "check-left-recursion"
       <> help "Detect left-recursive rules" )
    <*> switch
        ( long "suggest-shortcuts"
       <> help "Suggest common patterns for @shortcuts" )
    <*> optional (strOption
        ( long "expand-rule"
       <> metavar "RULENAME"
       <> help "Show fully expanded form of a rule" ))

    -- Output format
    <*> option (maybeReader parseFormat)
        ( long "debug-format"
       <> metavar "FORMAT"
       <> value FormatPretty
       <> help "Output format (pretty|compact|json|tree)" )
    <*> switch
        ( long "debug-color"
       <> help "Enable colored output (default: enabled)" )

    -- Performance profiling
    <*> switch
        ( long "profile-stages"
       <> help "Show timing for each compilation stage" )
    <*> switch
        ( long "memory-stats"
       <> help "Show memory usage per stage" )

    -- Export and logging
    <*> optional (strOption
        ( long "debug-output-dir"
       <> metavar "DIR"
       <> help "Write all debug outputs to directory" ))
    <*> optional (strOption
        ( long "debug-log"
       <> metavar "FILE"
       <> help "Write detailed debug log with timestamps" ))

    -- Special modes
    <*> switch
        ( long "interactive"
       <> short 'i'
       <> help "Step through compilation stages interactively" )
    <*> switch
        ( long "use-generated"
       <> help "Use generated parsers (experimental, for bootstrap testing)" )

-- | Parse command-line options
parseOptions :: IO DebugOptions
parseOptions = execParser opts
  where
    opts = info (debugOptionsParser <**> helper)
      ( fullDesc
     <> progDesc "RTK - Rewrite Toolkit for Grammar Development"
     <> header "rtk - a grammar-to-parser compiler with comprehensive debugging" )
