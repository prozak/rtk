module Debug
    ( -- * Debug output functions
      debugOutput
    , debugSection
    , debugSubSection

    -- * Pipeline stage debugging
    , debugTokens
    , debugInitialGrammar
    , debugNormalGrammar
    , debugComparison

    -- * Statistics and analysis
    , showGrammarStats
    , analyzeGrammarConflicts
    , showRuleList
    , showRuleGraph

    -- * Validation
    , validateGrammar
    , findUnusedRules
    , checkLeftRecursion
    , suggestShortcuts
    , expandRule

    -- * Performance profiling
    , timed
    , TimingInfo(..)
    , showTimingInfo

    -- * Output utilities
    , withColor
    , Color(..)
    ) where

import qualified Lexer as L
import Parser
import DebugOptions
import Text.Show.Pretty (ppShow)
import System.IO (hFlush, stdout)
import Data.Time.Clock (getCurrentTime, diffUTCTime, UTCTime)
import Data.List (intercalate, nub, (\\), sortBy)
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Maybe (fromMaybe, mapMaybe, isJust)
import Data.Ord (comparing)
import Control.Exception (evaluate)
import System.Mem (performGC)

import qualified System.Console.ANSI as ANSI

-------------------------------------------------------------------------------
-- Color support
-------------------------------------------------------------------------------

data Color = Red | Green | Yellow | Blue | Magenta | Cyan | White
    deriving (Eq, Show)

-- | Output text with color if enabled
withColor :: Bool -> Color -> String -> IO ()
withColor useColor color text = do
    when' useColor $ ANSI.setSGR [ANSI.SetColor ANSI.Foreground ANSI.Vivid (toANSIColor color)]
    putStr text
    when' useColor $ ANSI.setSGR [ANSI.Reset]
  where
    when' True action = action
    when' False _ = return ()

    toANSIColor Red = ANSI.Red
    toANSIColor Green = ANSI.Green
    toANSIColor Yellow = ANSI.Yellow
    toANSIColor Blue = ANSI.Blue
    toANSIColor Magenta = ANSI.Magenta
    toANSIColor Cyan = ANSI.Cyan
    toANSIColor White = ANSI.White

-------------------------------------------------------------------------------
-- Debug output formatting
-------------------------------------------------------------------------------

-- | Output debug information with optional formatting
debugOutput :: DebugOptions -> String -> String -> IO ()
debugOutput opts title content = do
    debugSection opts title
    putStrLn $ formatContent (debugFormat opts) content
    putStrLn ""

-- | Print a section header
debugSection :: DebugOptions -> String -> IO ()
debugSection opts title = do
    let separator = replicate 70 '='
    withColor (debugColor opts) Cyan $ separator ++ "\n"
    withColor (debugColor opts) Green $ "  " ++ title ++ "\n"
    withColor (debugColor opts) Cyan $ separator ++ "\n"

-- | Print a subsection header
debugSubSection :: DebugOptions -> String -> IO ()
debugSubSection opts title = do
    withColor (debugColor opts) Yellow $ "-- " ++ title ++ "\n"

-- | Format content according to debug format option
formatContent :: DebugFormat -> String -> String
formatContent FormatPretty s = s
formatContent FormatCompact s = filter (/= '\n') s
formatContent FormatJSON _ = "{}" -- Placeholder for JSON formatting
formatContent FormatTree s = s -- Could be enhanced with tree drawing

-------------------------------------------------------------------------------
-- Pipeline stage debugging
-------------------------------------------------------------------------------

-- | Debug tokens output
debugTokens :: DebugOptions -> [L.Token] -> IO ()
debugTokens opts tokens = do
    debugSection opts "LEXER OUTPUT - TOKENS"
    putStrLn $ "Total tokens: " ++ show (length tokens)
    putStrLn ""
    case debugFormat opts of
        FormatPretty -> putStrLn $ ppShow tokens
        FormatCompact -> putStrLn $ show tokens
        _ -> putStrLn $ ppShow tokens

-- | Debug initial grammar (after parsing)
debugInitialGrammar :: DebugOptions -> InitialGrammar -> IO ()
debugInitialGrammar opts grammar = do
    debugSection opts "PARSER OUTPUT - INITIAL GRAMMAR"
    putStrLn $ "Grammar name: " ++ getIGrammarName grammar
    putStrLn $ "Number of rules: " ++ show (length $ getIRules grammar)
    putStrLn ""
    case debugFormat opts of
        FormatPretty -> putStrLn $ ppShow grammar
        FormatCompact -> putStrLn $ show grammar
        _ -> putStrLn $ ppShow grammar

-- | Debug normalized grammar
debugNormalGrammar :: DebugOptions -> String -> NormalGrammar -> IO ()
debugNormalGrammar opts title grammar = do
    debugSection opts title
    putStrLn $ "Grammar name: " ++ getNGrammarName grammar
    putStrLn $ "Syntax rule groups: " ++ show (length $ getSyntaxRuleGroups grammar)
    putStrLn $ "Lexical rules: " ++ show (length $ getLexicalRules grammar)
    putStrLn $ "Anti-rules (QQ): " ++ show (length $ getAntiRules grammar)
    putStrLn ""
    case debugFormat opts of
        FormatPretty -> putStrLn $ ppShow grammar
        FormatCompact -> putStrLn $ show grammar
        _ -> putStrLn $ ppShow grammar

-- | Debug comparison between two values
debugComparison :: (Show a, Eq a) => DebugOptions -> String -> a -> String -> a -> IO ()
debugComparison opts title1 val1 title2 val2 = do
    debugSection opts $ "COMPARISON: " ++ title1 ++ " vs " ++ title2
    debugSubSection opts title1
    putStrLn $ ppShow val1
    putStrLn ""
    debugSubSection opts title2
    putStrLn $ ppShow val2
    putStrLn ""
    if val1 == val2
        then withColor (debugColor opts) Green "No differences found.\n"
        else withColor (debugColor opts) Yellow "Differences detected.\n"

-------------------------------------------------------------------------------
-- Statistics and analysis
-------------------------------------------------------------------------------

-- | Show comprehensive grammar statistics
showGrammarStats :: DebugOptions -> InitialGrammar -> NormalGrammar -> IO ()
showGrammarStats opts iGrammar nGrammar = do
    debugSection opts "GRAMMAR STATISTICS"

    let iRules = getIRules iGrammar
        sRuleGroups = getSyntaxRuleGroups nGrammar
        lRules = getLexicalRules nGrammar
        aRules = getAntiRules nGrammar
        shortcuts = getShortcuts nGrammar
        info = getGrammarInfo nGrammar
        proxyRules = getProxyRules info

        totalSyntaxRules = sum $ map (length . getSRules) sRuleGroups

    putStrLn $ "Grammar name: " ++ getNGrammarName nGrammar
    putStrLn ""
    putStrLn "=== Rule Counts ==="
    putStrLn $ "  Initial rules:        " ++ show (length iRules)
    putStrLn $ "  Syntax rule groups:   " ++ show (length sRuleGroups)
    putStrLn $ "  Total syntax rules:   " ++ show totalSyntaxRules
    putStrLn $ "  Lexical rules:        " ++ show (length lRules)
    putStrLn $ "  Anti-rules (QQ):      " ++ show (length aRules)
    putStrLn $ "  Proxy rules:          " ++ show (S.size proxyRules)
    putStrLn $ "  Shortcuts:            " ++ show (length shortcuts)
    putStrLn ""

    putStrLn "=== Constructor Information ==="
    let constructorCount = countConstructors sRuleGroups
    putStrLn $ "  Total constructors:   " ++ show constructorCount
    putStrLn ""

    putStrLn "=== Complexity Metrics ==="
    putStrLn $ "  Auto-generated rules: " ++ show (totalSyntaxRules - length iRules)
    putStrLn $ "  Name counter:         " ++ show (getNameCounter info)
    putStrLn ""

-- | Count constructors in syntax rules
countConstructors :: [SyntaxRuleGroup] -> Int
countConstructors groups = sum $ map countInGroup groups
  where
    countInGroup grp = sum $ map countInRule (getSRules grp)
    countInRule (SyntaxRule _ clause) = countInClause clause
    countInClause (STAltOfSeq seqs) = length seqs
    countInClause _ = 1

-- | Analyze grammar for potential conflicts
analyzeGrammarConflicts :: DebugOptions -> NormalGrammar -> IO ()
analyzeGrammarConflicts opts grammar = do
    debugSection opts "GRAMMAR CONFLICT ANALYSIS"

    let sRuleGroups = getSyntaxRuleGroups grammar
        lRules = getLexicalRules grammar

    debugSubSection opts "Potential Ambiguities"
    -- Check for rules with many alternatives
    let complexRules = filter hasManyAlternatives sRuleGroups
    if null complexRules
        then putStrLn "  No highly ambiguous rules found."
        else do
            putStrLn $ "  Found " ++ show (length complexRules) ++ " rules with many alternatives:"
            mapM_ (putStrLn . ("    - " ++) . getSDataTypeName) complexRules
    putStrLn ""

    debugSubSection opts "Lexical Token Conflicts"
    -- Check for overlapping string literals
    let stringLits = extractStringLiterals lRules
        duplicates = findDuplicates stringLits
    if null duplicates
        then putStrLn "  No duplicate string literals found."
        else do
            putStrLn $ "  Found " ++ show (length duplicates) ++ " duplicate literals:"
            mapM_ (putStrLn . ("    - " ++)) duplicates
    putStrLn ""

-- | Helper: Check if rule group has many alternatives
hasManyAlternatives :: SyntaxRuleGroup -> Bool
hasManyAlternatives grp = any checkRule (getSRules grp)
  where
    checkRule (SyntaxRule _ (STAltOfSeq seqs)) = length seqs > 10
    checkRule _ = False

-- | Helper: Extract string literals from lexical rules
extractStringLiterals :: [LexicalRule] -> [String]
extractStringLiterals = mapMaybe extractFromRule
  where
    extractFromRule (LexicalRule _ _ _ (IStrLit s)) = Just s
    extractFromRule _ = Nothing

-- | Helper: Find duplicates in a list
findDuplicates :: Eq a => [a] -> [a]
findDuplicates xs = xs \\ nub xs

-- | Show list of all rules
showRuleList :: DebugOptions -> NormalGrammar -> IO ()
showRuleList opts grammar = do
    debugSection opts "RULE LISTING"

    let sRuleGroups = getSyntaxRuleGroups grammar
        lRules = getLexicalRules grammar
        info = getGrammarInfo grammar
        proxyRules = getProxyRules info

    debugSubSection opts $ "Syntax Rules (" ++ show (length sRuleGroups) ++ " groups)"
    mapM_ (showRuleGroup opts proxyRules) sRuleGroups
    putStrLn ""

    debugSubSection opts $ "Lexical Rules (" ++ show (length lRules) ++ ")"
    mapM_ (showLexicalRule opts) lRules
    putStrLn ""

-- | Show a single rule group
showRuleGroup :: DebugOptions -> S.Set String -> SyntaxRuleGroup -> IO ()
showRuleGroup opts proxyRules grp = do
    let typeName = getSDataTypeName grp
        isProxy = S.member typeName proxyRules
        prefix = if isProxy then "  [PROXY] " else "  "
    withColor (debugColor opts) (if isProxy then Magenta else White) $ prefix ++ typeName
    putStrLn $ " (" ++ show (length $ getSRules grp) ++ " rules)"
    mapM_ (showSyntaxRule opts) (getSRules grp)

-- | Show a single syntax rule
showSyntaxRule :: DebugOptions -> SyntaxRule -> IO ()
showSyntaxRule opts (SyntaxRule name clause) = do
    putStrLn $ "    - " ++ name ++ ": " ++ summarizeClause clause

-- | Summarize a clause
summarizeClause :: SyntaxTopClause -> String
summarizeClause (STMany STStar _ _) = "list (*)"
summarizeClause (STMany STPlus _ _) = "list (+)"
summarizeClause (STOpt _) = "optional (?)"
summarizeClause (STAltOfSeq seqs) = show (length seqs) ++ " alternatives"

-- | Show a lexical rule
showLexicalRule :: DebugOptions -> LexicalRule -> IO ()
showLexicalRule opts (LexicalRule dtype _ name _) =
    putStrLn $ "  - " ++ name ++ " :: " ++ dtype
showLexicalRule opts (MacroRule name _) =
    putStrLn $ "  - " ++ name ++ " [MACRO]"

-- | Show rule dependency graph
showRuleGraph :: DebugOptions -> NormalGrammar -> IO ()
showRuleGraph opts grammar = do
    debugSection opts "RULE DEPENDENCY GRAPH"

    let sRuleGroups = getSyntaxRuleGroups grammar
        deps = buildDependencyMap sRuleGroups

    putStrLn "Rule -> Dependencies:"
    mapM_ (showDependency opts) (M.toList deps)

-- | Build dependency map
buildDependencyMap :: [SyntaxRuleGroup] -> M.Map String [String]
buildDependencyMap groups = M.fromList $ map buildForGroup groups
  where
    buildForGroup grp = (getSDataTypeName grp, extractDeps grp)
    extractDeps grp = nub $ concatMap (extractFromRule . getSClause) (getSRules grp)
    extractFromRule (STMany _ sc _) = extractFromSimple sc
    extractFromRule (STOpt sc) = extractFromSimple sc
    extractFromRule (STAltOfSeq seqs) = concatMap extractFromSeq seqs
    extractFromSeq (STSeq _ scs) = concatMap extractFromSimple scs
    extractFromSimple (SSId id) = [id]
    extractFromSimple (SSLifted id) = [id]
    extractFromSimple (SSIgnore id) = [id]

-- | Show a single dependency
showDependency :: DebugOptions -> (String, [String]) -> IO ()
showDependency opts (rule, deps) = do
    putStr $ "  " ++ rule ++ " -> "
    if null deps
        then putStrLn "[no dependencies]"
        else putStrLn $ intercalate ", " deps

-------------------------------------------------------------------------------
-- Validation
-------------------------------------------------------------------------------

-- | Validate grammar and report issues
validateGrammar :: DebugOptions -> NormalGrammar -> IO Bool
validateGrammar opts grammar = do
    debugSection opts "GRAMMAR VALIDATION"

    let sRuleGroups = getSyntaxRuleGroups grammar
        lRules = getLexicalRules grammar
        allSyntaxRuleNames = S.fromList $ concatMap (map getSRuleName . getSRules) sRuleGroups
        allLexicalRuleNames = S.fromList $ map getLRuleName lRules

    issues <- sequence
        [ checkUndefinedReferences opts sRuleGroups (allSyntaxRuleNames `S.union` allLexicalRuleNames)
        , checkDuplicateConstructors opts sRuleGroups
        ]

    let totalIssues = sum issues
    putStrLn ""
    if totalIssues == 0
        then do
            withColor (debugColor opts) Green "✓ Grammar validation passed!\n"
            return True
        else do
            withColor (debugColor opts) Red $ "✗ Found " ++ show totalIssues ++ " issue(s).\n"
            return False

-- | Check for undefined rule references
checkUndefinedReferences :: DebugOptions -> [SyntaxRuleGroup] -> S.Set String -> IO Int
checkUndefinedReferences opts groups allRules = do
    debugSubSection opts "Undefined References"
    let refs = S.fromList $ concatMap extractRefs groups
        undefined = S.toList $ refs `S.difference` allRules
    if null undefined
        then do
            putStrLn "  No undefined references."
            return 0
        else do
            putStrLn $ "  Found " ++ show (length undefined) ++ " undefined references:"
            mapM_ (putStrLn . ("    - " ++)) undefined
            return (length undefined)
  where
    extractRefs grp = concatMap (extractFromRule . getSClause) (getSRules grp)
    extractFromRule (STMany _ sc _) = extractFromSimple sc
    extractFromRule (STOpt sc) = extractFromSimple sc
    extractFromRule (STAltOfSeq seqs) = concatMap extractFromSeq seqs
    extractFromSeq (STSeq _ scs) = concatMap extractFromSimple scs
    extractFromSimple (SSId id) = [id]
    extractFromSimple (SSLifted id) = [id]
    extractFromSimple (SSIgnore id) = [id]

-- | Check for duplicate constructor names
checkDuplicateConstructors :: DebugOptions -> [SyntaxRuleGroup] -> IO Int
checkDuplicateConstructors opts groups = do
    debugSubSection opts "Duplicate Constructors"
    let constructors = concatMap extractConstructors groups
        duplicates = findDuplicates constructors
    if null duplicates
        then do
            putStrLn "  No duplicate constructors."
            return 0
        else do
            putStrLn $ "  Found " ++ show (length duplicates) ++ " duplicate constructors:"
            mapM_ (putStrLn . ("    - " ++)) duplicates
            return (length duplicates)
  where
    extractConstructors grp = concatMap extractFromRule (getSRules grp)
    extractFromRule (SyntaxRule _ (STAltOfSeq seqs)) = map (\(STSeq name _) -> name) seqs
    extractFromRule _ = []

-- | Find unused rules
findUnusedRules :: DebugOptions -> NormalGrammar -> IO ()
findUnusedRules opts grammar = do
    debugSection opts "UNUSED RULES"

    let sRuleGroups = getSyntaxRuleGroups grammar
        info = getGrammarInfo grammar
        startRule = getStartRuleName info
        allRules = M.fromList $ map (\g -> (getSDataTypeName g, g)) sRuleGroups
        deps = buildDependencyMap sRuleGroups

        reachable = case startRule of
            Just start -> findReachable start deps
            Nothing -> S.empty

        allRuleNames = M.keysSet allRules
        unused = S.toList $ allRuleNames `S.difference` reachable

    if null unused
        then putStrLn "  All rules are reachable from start rule."
        else do
            putStrLn $ "  Found " ++ show (length unused) ++ " unused rules:"
            mapM_ (putStrLn . ("    - " ++)) unused

-- | Find all reachable rules from a start rule
findReachable :: String -> M.Map String [String] -> S.Set String
findReachable start deps = go (S.singleton start) (S.singleton start)
  where
    go visited frontier
        | S.null frontier = visited
        | otherwise =
            let newNodes = S.fromList $ concatMap (\r -> fromMaybe [] (M.lookup r deps)) (S.toList frontier)
                unvisited = newNodes `S.difference` visited
            in go (visited `S.union` unvisited) unvisited

-- | Check for left recursion
checkLeftRecursion :: DebugOptions -> NormalGrammar -> IO ()
checkLeftRecursion opts grammar = do
    debugSection opts "LEFT RECURSION CHECK"

    let sRuleGroups = getSyntaxRuleGroups grammar
        leftRecursive = filter (isLeftRecursive sRuleGroups) sRuleGroups

    if null leftRecursive
        then putStrLn "  No left-recursive rules detected."
        else do
            putStrLn $ "  Found " ++ show (length leftRecursive) ++ " potentially left-recursive rules:"
            mapM_ (putStrLn . ("    - " ++) . getSDataTypeName) leftRecursive

-- | Check if a rule group is left-recursive
isLeftRecursive :: [SyntaxRuleGroup] -> SyntaxRuleGroup -> Bool
isLeftRecursive allGroups grp =
    let ruleName = getSDataTypeName grp
        firstSymbols = concatMap (getFirstSymbols . getSClause) (getSRules grp)
    in ruleName `elem` firstSymbols
  where
    getFirstSymbols (STAltOfSeq seqs) = concatMap getFirstFromSeq seqs
    getFirstSymbols (STMany _ sc _) = getFirstFromSimple sc
    getFirstSymbols (STOpt _) = []
    getFirstFromSeq (STSeq _ []) = []
    getFirstFromSeq (STSeq _ (sc:_)) = getFirstFromSimple sc
    getFirstFromSimple (SSId id) = [id]
    getFirstFromSimple (SSLifted id) = [id]
    getFirstFromSimple (SSIgnore _) = []

-- | Suggest shortcuts for common patterns
suggestShortcuts :: DebugOptions -> NormalGrammar -> IO ()
suggestShortcuts opts grammar = do
    debugSection opts "SHORTCUT SUGGESTIONS"

    let lRules = getLexicalRules grammar
        stringLits = extractStringLiterals lRules
        freq = countFrequencies stringLits
        common = filter ((> 2) . snd) freq

    if null common
        then putStrLn "  No common patterns found for shortcuts."
        else do
            putStrLn "  Common string literals (used > 2 times):"
            mapM_ (\(s, n) -> putStrLn $ "    - \"" ++ s ++ "\" (used " ++ show n ++ " times)") common

-- | Count frequencies of elements
countFrequencies :: Eq a => [a] -> [(a, Int)]
countFrequencies xs = map (\x -> (x, length $ filter (== x) xs)) (nub xs)

-- | Expand a rule by inlining all references
expandRule :: DebugOptions -> NormalGrammar -> String -> IO ()
expandRule opts grammar ruleName = do
    debugSection opts $ "EXPANDED RULE: " ++ ruleName

    let sRuleGroups = getSyntaxRuleGroups grammar
        maybeGroup = lookup ruleName $ map (\g -> (getSDataTypeName g, g)) sRuleGroups

    case maybeGroup of
        Nothing -> putStrLn $ "  Rule '" ++ ruleName ++ "' not found."
        Just grp -> do
            putStrLn $ "  Type: " ++ getSDataTypeName grp
            putStrLn $ "  Rules: " ++ show (length $ getSRules grp)
            putStrLn ""
            mapM_ (showExpandedRule opts) (getSRules grp)

-- | Show an expanded syntax rule
showExpandedRule :: DebugOptions -> SyntaxRule -> IO ()
showExpandedRule opts (SyntaxRule name clause) = do
    putStrLn $ "  " ++ name ++ ":"
    putStrLn $ ppShow clause
    putStrLn ""

-------------------------------------------------------------------------------
-- Performance profiling
-------------------------------------------------------------------------------

-- | Timing information for a stage
data TimingInfo = TimingInfo
    { stageName :: String
    , startTime :: UTCTime
    , endTime :: UTCTime
    } deriving (Show)

-- | Execute an action and time it
timed :: String -> IO a -> IO (a, TimingInfo)
timed name action = do
    start <- getCurrentTime
    result <- action
    _ <- evaluate result  -- Force evaluation
    end <- getCurrentTime
    return (result, TimingInfo name start end)

-- | Show timing information
showTimingInfo :: DebugOptions -> [TimingInfo] -> IO ()
showTimingInfo opts timings = do
    debugSection opts "STAGE TIMING PROFILE"

    let totalTime = sum $ map (\t -> realToFrac $ diffUTCTime (endTime t) (startTime t)) timings

    putStrLn $ "  Total time: " ++ formatTime totalTime
    putStrLn ""
    putStrLn "  Stage breakdown:"
    mapM_ (showSingleTiming opts) timings
  where
    formatTime :: Double -> String
    formatTime t
        | t < 0.001 = show (round $ t * 1000000) ++ "μs"
        | t < 1.0 = show (round $ t * 1000) ++ "ms"
        | otherwise = show (round t) ++ "s"

    showSingleTiming opts timing = do
        let duration = realToFrac $ diffUTCTime (endTime timing) (startTime timing)
        putStrLn $ "    " ++ stageName timing ++ ": " ++ formatTime duration
