{-# LANGUAGE QuasiQuotes #-}
-- | Unit tests for the rtk front end:
--
--   * StrQuote quasi-quoter behavior (ported from StrQuote_Test.hs)
--   * token post-processing
--   * pipeline error handling (ported from EmptyGrammar_Test.hs)
--   * normalization behavior on small inline grammars
--   * normalization invariants checked against every grammar in test-grammars/
module Main (main) where

import Control.Exception (ErrorCall (..), SomeException, catch, evaluate, try)
import Control.Monad (when)
import Data.List (find, group, isInfixOf, isPrefixOf, nub, sort)
import qualified Data.Map as M
import Data.Maybe (maybeToList)
import qualified Data.Set as S
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import Test.HUnit

import Grammar (isClauseSeqLifted)
import Lexer (AlexPosn (..), PosToken (..), Token (..))
import Normalize (fillConstructorNames, normalizeTopLevelClauses)
import Parser
import StrQuote (str)
import StringLiterals (normalizeStringLiterals)
import TokenProcessing (catBigstrs, processTokens, unBackQuote)

import TestSupport

main :: IO ()
main = do
    pgFiles <- discoverGrammarFiles grammarsDir
    perGrammar <- mapM invariantTestsFor pgFiles
    results <- runTestTT $ TestList $
        [ TestLabel "StrQuote" strQuoteTests
        , TestLabel "TokenProcessing" tokenProcessingTests
        , TestLabel "pipeline error handling" errorHandlingTests
        , TestLabel "normalization behavior" normalizationTests
        ] ++ perGrammar
    when (errors results + failures results /= 0) exitFailure

-- | Normalize without constructor-name filling, for tests that inspect the
-- intermediate form.
normalizeNoFill :: String -> NormalGrammar
normalizeNoFill = normalizeTopLevelClauses . normalizeStringLiterals . parseGrammarSource

--------------------------------------------------------------------------------
-- StrQuote
--------------------------------------------------------------------------------

strQuoteTests :: Test
strQuoteTests = TestList
    [ TestCase $ assertEqual "simple string" "simple string" [str|simple string|]
    , TestCase $ assertEqual "string with newline" "simple string\nwith new line" [str|simple string
with new line|]
    , TestCase $ assertEqual "empty string" "" [str||]
    , TestCase $ assertEqual "empty var name" "<empty var name>" [str|?|]
    , TestCase $ assertEqual "empty expr" "<empty expr>" [str|?()|]
    ]

--------------------------------------------------------------------------------
-- TokenProcessing
--------------------------------------------------------------------------------

tokenProcessingTests :: Test
tokenProcessingTests = TestList
    [ TestLabel "unBackQuote strips escaping backslashes" $ TestCase $
        assertEqual "" "a'b" (unBackQuote "a\\'b")
    , TestLabel "unBackQuote keeps \\n \\t \\r escapes" $ TestCase $
        assertEqual "" "\\n\\t\\r" (unBackQuote "\\n\\t\\r")
    , TestLabel "unBackQuote unescapes backslash itself" $ TestCase $
        assertEqual "" "\\" (unBackQuote "\\\\")
    , TestLabel "catBigstrs joins adjacent big strings" $ TestCase $
        assertEqual "" (map at [BigStr "a\nb", Id "x"])
                       (catBigstrs (map at [BigStr "a", BigStr "b", Id "x"]))
    , TestLabel "catBigstrs keeps the position of the first part" $ TestCase $
        assertEqual "" [PosToken (AlexPn 0 1 1) (BigStr "a\nb")]
                       (catBigstrs [ PosToken (AlexPn 0 1 1) (BigStr "a")
                                   , PosToken (AlexPn 5 2 1) (BigStr "b") ])
    , TestLabel "processTokens combines both steps" $ TestCase $
        assertEqual "" (map at [StrLit "'", BigStr "a\nb"])
                       (processTokens (map at [StrLit "\\'", BigStr "a", BigStr "b"]))
    ]

-- | Wrap a token at a dummy position; token processing ignores positions.
at :: Token -> PosToken
at = PosToken (AlexPn 0 1 1)

--------------------------------------------------------------------------------
-- Pipeline error handling
--------------------------------------------------------------------------------

expectErrorCall :: a -> IO (Either String String)
expectErrorCall value =
    catch (evaluate value >> return (Left "no error thrown"))
          (\(ErrorCall msg) -> return (Right msg))

errorHandlingTests :: Test
errorHandlingTests = TestList
    [ TestLabel "grammar without rules is rejected" $ TestCase $ do
        result <- expectErrorCall $ normalizeNoFill "grammar 'Empty';"
        case result of
            Left err -> assertFailure $ "expected an error about the empty grammar, got: " ++ err
            Right msg -> assertBool ("unexpected message: " ++ msg) $
                "contains no rules" `isInfixOf` msg && "Empty" `isInfixOf` msg
    , TestLabel "empty input is a parse error" $ TestCase $ do
        result <- expectErrorCall $ parseGrammarSource ""
        case result of
            Left err -> assertFailure $ "expected a parse error for empty input, got: " ++ err
            Right msg -> assertBool ("unexpected message: " ++ msg) $
                "Parse error" `isInfixOf` msg
    , TestLabel "minimal valid grammar normalizes" $ TestCase $ do
        result <- expectErrorCall $ forceGrammar $
            normalizeGrammarSource "grammar 'Valid';\nRule = 'test' ;"
        case result of
            Left _ -> return ()
            Right msg -> assertFailure $ "valid grammar should normalize, got error: " ++ msg
    , TestLabel "parse errors report the offending position" $ TestCase $ do
        -- ';' missing after the grammar declaration: the parser should point
        -- at the identifier 'Foo' on line 2
        result <- expectErrorCall $ parseGrammarSource "grammar 'Test'\nFoo = bar;\n"
        case result of
            Left err -> assertFailure $ "expected a parse error, got: " ++ err
            Right msg -> assertBool ("unexpected message: " ++ msg) $
                "line 2, column 1" `isInfixOf` msg && "identifier 'Foo'" `isInfixOf` msg
    , TestLabel "errors at end of input carry a position" $ TestCase $ do
        result <- expectErrorCall $ parseGrammarSource "grammar 'Test';\nFoo =\n"
        case result of
            Left err -> assertFailure $ "expected a parse error, got: " ++ err
            Right msg -> assertBool ("unexpected message: " ++ msg) $
                "line 3, column 1" `isInfixOf` msg && "end of input" `isInfixOf` msg
    , TestLabel "a lexical first rule is rejected with an explanation" $ TestCase $ do
        result <- expectErrorCall $ forceGrammar $
            normalizeGrammarSource "grammar 'Test';\nfoo = [a-z];\n"
        case result of
            Left err -> assertFailure $ "expected an error about the first rule, got: " ++ err
            Right msg -> assertBool ("unexpected message: " ++ msg) $
                "must be a syntax rule" `isInfixOf` msg && "foo" `isInfixOf` msg
    , TestLabel "normalization errors name the offending rule and position" $ TestCase $ do
        -- a lifted (,) clause mixed with other clauses is rejected; the error
        -- should point at rule 'Foo' on line 2
        result <- expectErrorCall $ forceGrammar $
            normalizeGrammarSource "grammar 'Test';\nFoo = ,Bar Baz;\nBar = 'b';\nBaz = 'z';\n"
        case result of
            Left err -> assertFailure $ "expected an error about the lifted clause, got: " ++ err
            Right msg -> assertBool ("unexpected message: " ++ msg) $
                "in rule 'Foo'" `isInfixOf` msg && "line 2" `isInfixOf` msg
                && "lifted" `isInfixOf` msg
    ]

-- | Force the whole grammar value so lazy errors surface where we expect them.
forceGrammar :: NormalGrammar -> NormalGrammar
forceGrammar g = length (show g) `seq` g

--------------------------------------------------------------------------------
-- Normalization behavior on small inline grammars
--------------------------------------------------------------------------------

normalizationTests :: Test
normalizationTests = TestList
    [ TestLabel "string literals become shared ignored keyword tokens" testStringLiterals
    , TestLabel "list rules get an element proxy with QQ splicing support" testListProxy
    , TestLabel "QQ anti machinery is created once per shared type" testAntiRuleSharing
    , TestLabel "shortcuts are recorded against the rule's type" testShortcuts
    , TestLabel "optional clauses desugar to an empty alternative" testOptionalDesugars
    , TestLabel "lexical rule type and conversion defaults" testLexicalRuleDefaults
    , TestLabel "start group wraps every public type in dummy tokens" testStartGroup
    , TestLabel "fillConstructorNames fills every constructor and is idempotent" testFillConstructorNames
    ]

testStringLiterals :: Test
testStringLiterals = TestCase $ do
    let g = normalizeStringLiterals $ parseGrammarSource $ unlines
                [ "grammar 'Lit';"
                , "A = 'x' bb ;"
                , "B = 'x' A ;"
                , "bb = [b]+ ;"
                ]
        tokenRules = [ r | r <- getIRules g, getIClause r == IStrLit "x" ]
    -- both uses of 'x' share a single generated keyword rule
    case tokenRules of
        [tok] -> do
            assertEqual "keyword token type" (Just "Keyword") (getIDataTypeName tok)
            let tokName = getIRuleName tok
                clauseOf n = getIClause <$> find ((== n) . getIRuleName) (getIRules g)
            assertEqual "literal in A replaced by ignored token"
                (Just $ IAlt [ISeq [IIgnore (IId tokName), IId "bb"]]) (clauseOf "A")
            assertEqual "literal in B replaced by ignored token"
                (Just $ IAlt [ISeq [IIgnore (IId tokName), IId "A"]]) (clauseOf "B")
        _ -> assertFailure $ "expected exactly one token rule for 'x', got: " ++ show tokenRules

listGrammar :: NormalGrammar
listGrammar = normalizeNoFill $ unlines
    [ "grammar 'Mini';"
    , "Program = Item* ;"
    , "Item = ident ;"
    , "ident = [a-z]+ ;"
    ]

testListProxy :: Test
testListProxy = TestCase $ do
    let g = listGrammar
    -- the user's list rule now points at a generated proxy element rule
    proxyName <- case [ r | r <- allRules g, getSRuleName r == "Program"
                          , STMany{} <- [getSClause r] ] of
        [SyntaxRule _ (STMany STStar (SSId proxy) Nothing)] -> return proxy
        other -> fail $ "expected Program to stay a star rule over a proxy, got: " ++ show other
    assertBool ("proxy named after the list rule: " ++ proxyName)
        ("ListElem_Program" `isPrefixOf` proxyName)
    -- the proxy parses either a $Program:var splice or a real (lifted) element
    case find ((== proxyName) . getSRuleName) (allRules g) of
        Just (SyntaxRule _ (STAltOfSeq [STSeq anti [SSId qqTok], STSeq "" [SSLifted "Item"]])) -> do
            assertEqual "anti constructor lives in the element type" "Anti_Item" anti
            assertEqual "QQ token is named after the list rule" "qq_Program" qqTok
        other -> assertFailure $ "unexpected proxy rule shape: " ++ show other
    -- the proxy is grouped under the element's type, and the anti rule splices lists
    assertEqual "group of the proxy rule" ["Item"]
        [ getSDataTypeName grp | grp <- getSyntaxRuleGroups g
                               , proxyName `elem` map getSRuleName (getSRules grp) ]
    assertEqual "anti rules" [AntiRule "Item" "Item" "Anti_Item" True] (getAntiRules g)

testAntiRuleSharing :: Test
testAntiRuleSharing = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Shared';"
                , "Top = Expr ;"
                , "Expr: Add = aa ;"
                , "Expr: Mul = bb ;"
                , "aa = [a]+ ;"
                , "bb = [b]+ ;"
                ]
    assertEqual "one anti rule per type"
        ["Expr", "Top"] (sort $ map arTypeName $ getAntiRules g)
    assertEqual "one QQ lexical token per type"
        ["qq_Expr", "qq_Top"]
        (sort [ n | LexicalRule{getLRuleName = n} <- getLexicalRules g, "qq_" `isPrefixOf` n ])
    -- both rules of the shared type accept the same splice alternative
    let antiAltsOf name = [ alt | SyntaxRule rn (STAltOfSeq alts) <- allRules g, rn == name
                                , alt@(STSeq "Anti_Expr" _) <- alts ]
    assertEqual "Add accepts $Expr: splices" [STSeq "Anti_Expr" [SSId "qq_Expr"]] (antiAltsOf "Add")
    assertEqual "Mul accepts $Expr: splices" [STSeq "Anti_Expr" [SSId "qq_Expr"]] (antiAltsOf "Mul")

testShortcuts :: Test
testShortcuts = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Cut';"
                , "Top = Expr ;"
                , "@shortcuts(e, ex)"
                , "Expr = ident ;"
                , "ident = [a-z]+ ;"
                ]
    assertEqual "shortcuts map to the rule's type"
        [("e", "Expr"), ("ex", "Expr")] (sort $ getShortcuts g)

testOptionalDesugars :: Test
testOptionalDesugars = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Opt';"
                , "Top = S ;"
                , "S = ident? ;"
                , "ident = [a-z]+ ;"
                ]
    case [ alts | SyntaxRule "S" (STAltOfSeq alts) <- allRules g ] of
        [alts] -> assertEqual "splice, absent and present alternatives"
            [STSeq "Anti_S" [SSId "qq_S"], STSeq "" [], STSeq "" [SSId "ident"]] alts
        other -> assertFailure $ "unexpected S alternatives: " ++ show other

testLexicalRuleDefaults :: Test
testLexicalRuleDefaults = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Lex';"
                , "S = w ;"
                , "w = [a]+ ;"
                , "Int: x = [0-9]+ ;"
                , "Int.toInteger: y = [0-9]+ ;"
                , "@symmacro"
                , "z = [c] ;"
                ]
        ruleOf n = find ((== n) . getLRuleName) (getLexicalRules g)
        typeAndFunc n = case ruleOf n of
            Just LexicalRule{getLRuleDataType = t, getLRuleFunc = f} -> Just (t, f)
            _ -> Nothing
    assertEqual "untyped rule is a String with id" (Just ("String", "id")) (typeAndFunc "w")
    assertEqual "typed rule defaults to read" (Just ("Int", "read")) (typeAndFunc "x")
    assertEqual "explicit conversion function wins" (Just ("Int", "toInteger")) (typeAndFunc "y")
    case ruleOf "z" of
        Just MacroRule{} -> return ()
        other -> assertFailure $ "@symmacro should produce a macro rule, got: " ++ show other

testStartGroup :: Test
testStartGroup = TestCase $ do
    let g = listGrammar
        info = getGrammarInfo g
        startInfo = getRuleToStartInfo info
    assertEqual "start rule is the first user rule" (Just "Program") (getStartRuleName info)
    assertEqual "every public type gets a start entry"
        ["Item", "Program"] (sort $ M.keys startInfo)
    -- the synthesized start rule wraps each type between its dummy tokens
    -- (compared via show because STSeq has no Ord instance)
    case [ alts | SyntaxRule "Program" (STAltOfSeq alts) <- getSRules (head (getSyntaxRuleGroups g)) ] of
        [alts] -> assertEqual "wrapper alternatives"
            (sort [ show (STSeq "" [SSIgnore d, SSId t, SSIgnore d]) | (t, d) <- M.toList startInfo ])
            (sort (map show alts))
        other -> assertFailure $ "expected one synthesized start rule, got: " ++ show other
    -- each dummy token is lexed as a keyword spelled like its own name
    mapM_ (\dummy -> case find ((== dummy) . getLRuleName) (getLexicalRules g) of
              Just LexicalRule{getLRuleDataType = t, getLClause = cl} -> do
                  assertEqual ("dummy token type for " ++ dummy) "Keyword" t
                  assertEqual ("dummy token spelling for " ++ dummy) (IStrLit dummy) cl
              other -> assertFailure $ "missing dummy keyword " ++ dummy ++ ": " ++ show other)
          (M.elems startInfo)

testFillConstructorNames :: Test
testFillConstructorNames = TestCase $ do
    let g = listGrammar
        filled = fillConstructorNames g
    assertEqual "no unnamed constructors remain" [] (unnamedConstructors filled)
    assertBool "anti constructors survive filling"
        ("Anti_Item" `elem` [ c | STSeq c _ <- allSeqs filled ])
    assertEqual "filling is idempotent" filled (fillConstructorNames filled)

--------------------------------------------------------------------------------
-- Invariants checked against every grammar in test-grammars/
--------------------------------------------------------------------------------

invariantTestsFor :: FilePath -> IO Test
invariantTestsFor pgFile = do
    source <- readFileUtf8 pgFile
    normalized <- try (evaluate (forceGrammar (normalizeNoFill source)))
    return $ TestLabel (takeBaseName pgFile ++ " invariants") $ case normalized of
        Left err -> TestCase $ assertFailure $
            "normalization failed for " ++ pgFile ++ ":\n" ++ show (err :: SomeException)
        Right g -> invariants (takeBaseName pgFile) g

-- | Rule names that a grammar deliberately defines more than once.
-- debug-test.pg defines IfStatement twice to exercise the debug options; it is
-- only used for rtk's own diagnostics and is never fed to happy.
knownDuplicateRuleNames :: String -> [ID]
knownDuplicateRuleNames "debug-test" = ["IfStatement"]
knownDuplicateRuleNames _ = []

-- | References to rules that a grammar is known to leave undefined.
-- Currently empty: every grammar in test-grammars/ resolves all its
-- references. Pin a grammar here only to keep the invariant active for the
-- others while a known defect is being worked on.
knownUnresolvedReferences :: String -> [ID]
knownUnresolvedReferences _ = []

invariants :: String -> NormalGrammar -> Test
invariants grammarKey g = TestList
    [ TestLabel "syntax rule names are unique" $ TestCase $
        -- the synthesized start wrapper legitimately shares the start rule's name
        assertEqual "duplicate rule names" [] $
            duplicates (map getSRuleName (allRules g))
                `removeAll` (maybeToList (getStartRuleName info)
                             ++ knownDuplicateRuleNames grammarKey)
    , TestLabel "rule group types are unique" $ TestCase $
        assertEqual "duplicate group types" [] $
            duplicates (map getSDataTypeName (getSyntaxRuleGroups g))
    , TestLabel "token names are unique" $ TestCase $
        assertEqual "duplicate token names" [] $
            duplicates [ getLRuleName lr | lr@LexicalRule{} <- getLexicalRules g ]
    , TestLabel "every clause reference resolves" $ TestCase $ do
        let defined = S.fromList (map getSRuleName (allRules g)) `S.union` usableTokens
            missing = [ ref
                      | r <- allRules g
                      , ref <- map clauseRef (clauseElems (getSClause r))
                      , not (ref `S.member` defined) ]
        assertEqual "references to unknown rules or tokens"
            (knownUnresolvedReferences grammarKey) (sort (nub missing))
    , TestLabel "anti rules are unique per type" $ TestCase $
        assertEqual "duplicate anti rules" [] (duplicates (map arTypeName (getAntiRules g)))
    , TestLabel "lifted clauses are well-formed" $ TestCase $ do
        checked <- try (evaluate (length (filter (isClauseSeqLifted . seqClauses) (allSeqs g))))
        case checked of
            Left err -> assertFailure $ "isClauseSeqLifted rejected a sequence: "
                                        ++ show (err :: SomeException)
            Right _ -> return ()
    , TestLabel "constructors are filled and unambiguous" $ TestCase $ do
        let filled = fillConstructorNames g
        assertEqual "unnamed constructors" [] (unnamedConstructors filled)
        assertEqual "constructors with conflicting field types" [] (ambiguousConstructors filled)
        assertEqual "filling is idempotent" filled (fillConstructorNames filled)
    , TestLabel "start rule bookkeeping is consistent" $ TestCase $
        case getStartRuleName info of
            Nothing -> assertFailure "start rule name is missing"
            Just startName -> do
                assertEqual "start wrapper and original rule"
                    2 (length [ () | r <- allRules g, getSRuleName r == startName ])
                assertEqual "start info covers exactly the public types"
                    (sort (map getSDataTypeName publicGroups))
                    (sort (M.keys (getRuleToStartInfo info)))
    , TestLabel "proxy rules refer to existing groups" $ TestCase $
        assertEqual "unknown proxy rules" [] $
            S.toList (getProxyRules info)
                `removeAll` map getSDataTypeName (getSyntaxRuleGroups g)
    ]
  where
    info = getGrammarInfo g
    publicGroups = filterProxyRules (getProxyRules info) (getSyntaxRuleGroups g)
    -- tokens that may be referenced from syntax rules: macro rules are inlined
    -- into the lexer spec and Ignore tokens are dropped from the token stream,
    -- so neither may appear in a parser rule
    usableTokens = S.fromList [ name | LexicalRule{getLRuleName = name, getLRuleDataType = dt} <- getLexicalRules g
                              , dt /= "Ignore" ]

--------------------------------------------------------------------------------
-- Small helpers over the normalized grammar
--------------------------------------------------------------------------------

allRules :: NormalGrammar -> [SyntaxRule]
allRules = concatMap getSRules . getSyntaxRuleGroups

allSeqs :: NormalGrammar -> [STSeq]
allSeqs g = [ s | SyntaxRule _ (STAltOfSeq alts) <- allRules g, s <- alts ]

seqClauses :: STSeq -> [SyntaxSimpleClause]
seqClauses (STSeq _ cs) = cs

clauseElems :: SyntaxTopClause -> [SyntaxSimpleClause]
clauseElems (STMany _ c mc) = c : maybeToList mc
clauseElems (STOpt c) = [c]
clauseElems (STAltOfSeq alts) = concatMap seqClauses alts

clauseRef :: SyntaxSimpleClause -> ID
clauseRef (SSId i) = i
clauseRef (SSLifted i) = i
clauseRef (SSIgnore i) = i

unnamedConstructors :: NormalGrammar -> [STSeq]
unnamedConstructors g = [ s | s@(STSeq "" _) <- allSeqs g ]

-- | Constructor names that would be generated with conflicting field types.
-- GenAST deduplicates alternatives by constructor name (shared types see the
-- same Anti_ alternative from several rules), which is only sound if every
-- occurrence of a constructor has the same field types.
ambiguousConstructors :: NormalGrammar -> [(ConstructorName, [[ID]])]
ambiguousConstructors g =
    [ (name, S.toList fieldVariants)
    | (name, fieldVariants) <- M.toList byConstructor, S.size fieldVariants > 1 ]
  where
    byConstructor = M.fromListWith S.union
        [ (name, S.singleton (fieldTypes cs))
        | grp <- getSyntaxRuleGroups g
        , SyntaxRule _ (STAltOfSeq alts) <- getSRules grp
        , STSeq name cs <- alts
        , not (isClauseSeqLifted cs) ]
    fieldTypes cs = [ M.findWithDefault ("?" ++ i) i typeOf | SSId i <- cs ]
    typeOf = M.fromList $
        [ (name, dt) | LexicalRule{getLRuleName = name, getLRuleDataType = dt} <- getLexicalRules g ]
        ++ [ (getSRuleName r, getSDataTypeName grp)
           | grp <- getSyntaxRuleGroups g, r <- getSRules grp ]

duplicates :: Ord a => [a] -> [a]
duplicates = map head . filter ((> 1) . length) . group . sort

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs banned = [ x | x <- xs, x `notElem` banned ]
