{-# LANGUAGE QuasiQuotes #-}
-- | Unit tests for the rtk front end:
--
--   * StrQuote quasi-quoter behavior (ported from StrQuote_Test.hs)
--   * token post-processing
--   * pipeline error handling (ported from EmptyGrammar_Test.hs)
--   * normalization behavior on small inline grammars
--   * normalization invariants checked against every grammar in test-grammars/
--   * the compiled-in generated quasi-quoter (GrammarQQ smoke test)
module Main (main) where

import Control.Exception (SomeException, evaluate, try)
import Control.Monad (when)
import Data.List (find, group, isInfixOf, isPrefixOf, nub, sort)
import qualified Data.Map as M
import Data.Maybe (listToMaybe, maybeToList)
import qualified Data.Set as S
import System.Exit (exitFailure)
import System.FilePath (takeBaseName)
import Test.HUnit

import Data.Generics (everything, mkQ)
import qualified GrammarLexer as GL
import qualified GrammarParser as GP

import Diagnostics (Diagnostic (..), SourcePos (..), renderDiagnostic)
import Frontend (grammarRules, ruleClause, ruleName, ruleTypeName)
import GenX (isAlexEscape)
import Grammar (isClauseSeqLifted)
import GrammarParser (Clause)
import GrammarQQ (clause)
import Lexer (AlexPosn (..), PosToken (..), Token (..))
import Normalize (fillConstructorNames, normalizeTopLevelClauses)
import Syntax
import StrQuote (str)
import StringLiterals (normalizeStringLiterals)
import TokenProcessing (catBigstrs, processTokens, unBackQuote)

import TestSupport

main :: IO ()
main = do
    pgFiles <- discoverGrammarFiles grammarsDir
    perGrammar <- mapM invariantTestsFor pgFiles
    astEquality <- mapM astEqualityTestFor pgFiles
    results <- runTestTT $ TestList $
        [ TestLabel "StrQuote" strQuoteTests
        , TestLabel "TokenProcessing" tokenProcessingTests
        , TestLabel "diagnostics" diagnosticsTests
        , TestLabel "pipeline error handling" errorHandlingTests
        , TestLabel "normalization behavior" normalizationTests
        , TestLabel "named constructors" namedConstructorTests
        , TestLabel "self-hosted front end" selfHostedFrontEndTests
        , TestLabel "compiled-in quasi-quoter (GrammarQQ)" grammarQQTests
        ] ++ perGrammar ++ astEquality
    when (errors results + failures results /= 0) exitFailure

-- | Normalize without constructor-name filling, returning the diagnostic on
-- failure. Used by the error-handling tests.
normalizeNoFillE :: String -> Either Diagnostic NormalGrammar
normalizeNoFillE src = parseGrammarSource src >>= (normalizeTopLevelClauses . normalizeStringLiterals)

-- | Partial unwrapping for the behavior tests, which all use valid grammars:
-- a diagnostic here means the test grammar itself is wrong.
normalizeNoFill :: String -> NormalGrammar
normalizeNoFill = either (error . ("unexpected diagnostic: " ++) . show) id . normalizeNoFillE

-- | Parse a valid grammar, failing loudly on a diagnostic.
parseOrDie :: String -> GP.Grammar
parseOrDie = either (error . ("unexpected diagnostic: " ++) . show) id . parseGrammarSource

--------------------------------------------------------------------------------
-- Generated-AST builders for expected values. Positions are
-- equality-transparent, so rtkNoPos compares equal to any parsed position.
--------------------------------------------------------------------------------

np :: GP.RtkPos
np = GP.rtkNoPos

gpRef :: String -> GP.Clause
gpRef n = GP.Ref np (GP.Ident np n)

gpLit :: String -> GP.Clause
gpLit s = GP.Lit np (GP.Str np s)

gpIgnore :: GP.Clause -> GP.Clause
gpIgnore = GP.Ignored np

gpSeq :: [GP.Clause] -> GP.Clause
gpSeq = foldl1 (GP.Seq np)

gpAlt :: [GP.Clause] -> GP.Clause
gpAlt = foldl1 (GP.Alt np)

gpLabeled :: String -> GP.Clause -> GP.Clause
gpLabeled n = GP.Labeled np (GP.Ident np n)

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
    , TestLabel "unBackQuote keeps \\n \\t \\r \\f \\v escapes" $ TestCase $
        assertEqual "" "\\n\\t\\r\\f\\v" (unBackQuote "\\n\\t\\r\\f\\v")
    , TestLabel "preserved escapes are exactly the ones GenX emits bare" $ TestCase $
        mapM_ (\c -> do
            let esc = ['\\', c]
            assertEqual ("unBackQuote preserves " ++ esc) esc (unBackQuote esc)
            assertBool ("GenX.isAlexEscape recognizes " ++ esc) (isAlexEscape esc))
          "ntrfv"
    , TestLabel "unBackQuote unescapes backslash itself" $ TestCase $
        assertEqual "" "\\" (unBackQuote "\\\\")
    , TestLabel "catBigstrs joins adjacent big strings" $ TestCase $
        -- tokens carry the raw text: merging drops the inner delimiters and
        -- yields a well-delimited block again
        assertEqual "" (map at [BigStr "\"\"\"a\nb\"\"\"", Id "x"])
                       (catBigstrs (map at [BigStr "\"\"\"a\"\"\"", BigStr "\"\"\"b\"\"\"", Id "x"]))
    , TestLabel "catBigstrs keeps the position of the first part" $ TestCase $
        assertEqual "" [PosToken (AlexPn 0 1 1) (BigStr "\"\"\"a\nb\"\"\"")]
                       (catBigstrs [ PosToken (AlexPn 0 1 1) (BigStr "\"\"\"a\"\"\"")
                                   , PosToken (AlexPn 9 2 1) (BigStr "\"\"\"b\"\"\"") ])
    , TestLabel "processTokens merges big strings and leaves literals raw" $ TestCase $
        -- escape processing happens on the parsed AST
        -- (Frontend.cleanGrammarTokens), not here
        assertEqual "" (map at [StrLit "'\\''", BigStr "\"\"\"a\nb\"\"\""])
                       (processTokens (map at [StrLit "'\\''", BigStr "\"\"\"a\"\"\"", BigStr "\"\"\"b\"\"\""]))
    , TestLabel "a '\\f' keyword survives to the generated lexer" testFormFeedReachesLexer
    , TestLabel "a backslash in a character class is emitted Alex-escaped" testBackslashClassEscaped
    ]

-- | End-to-end: a '\f' keyword must reach the generated Alex spec as the bare
-- \f escape (neither stripped to a literal 'f' nor quoted as "\f", which Alex
-- would read as backslash + 'f').
testFormFeedReachesLexer :: Test
testFormFeedReachesLexer = TestCase $
    case normalizeGrammarSource "grammar 'Esc';\nS = '\\f' ;\n" >>= artifactsFor of
        Left d -> assertFailure $ "generation failed: " ++ show d
        Right artifacts -> case lookup "EscLexer.x" artifacts of
            Nothing -> assertFailure "no EscLexer.x artifact generated"
            Just lexerSpec -> do
                assertBool "lexer spec contains the bare \\f escape"
                    ("\\f" `isInfixOf` lexerSpec)
                assertBool "\\f is not emitted as a quoted string literal"
                    (not ("\"\\f\"" `isInfixOf` lexerSpec))

-- | End-to-end (issue #95): a literal backslash in a regex character class
-- must reach the generated Alex spec escaped ([\\]). It used to pass through
-- raw, where Alex set syntax reads a lone backslash as an escape, so users
-- needed the [\x5C] hex spelling. The \n \t \r \f \v pairs that token
-- post-processing preserves must keep passing through bare - including
-- directly after an escaped literal backslash.
testBackslashClassEscaped :: Test
testBackslashClassEscaped = TestCase $
    case normalizeGrammarSource src >>= artifactsFor of
        Left d -> assertFailure $ "generation failed: " ++ show d
        Right artifacts -> case lookup "BsLexer.x" artifacts of
            Nothing -> assertFailure "no BsLexer.x artifact generated"
            Just lexerSpec -> do
                assertBool "literal backslash class is emitted as [\\\\]"
                    ("[\\\\]" `isInfixOf` lexerSpec)
                assertBool "preserved control-character pairs stay bare in classes"
                    ("[\\ \\t\\n]+" `isInfixOf` lexerSpec)
                assertBool "negated class mixing both kinds keeps each one intact"
                    ("[^\\\"\\\\\\n\\r]" `isInfixOf` lexerSpec)
  where
    -- the backslash class must end its source line: the grammar lexer reads
    -- '\]' as an escape pair, so [\\] followed by another ']' on the same
    -- line would mis-lex (see the backslash macro comment in java.pg)
    src = unlines
        [ "grammar 'Bs';"
        , "S = cls tab nq ;"
        , "cls = 'x' [\\\\] ;"
        , "tab = [ \\t\\n]+ ;"
        , "nq = [^\\\"\\\\\\n\\r] ;"
        ]

-- | Wrap a token at a dummy position; token processing ignores positions.
at :: Token -> PosToken
at = PosToken (AlexPn 0 1 1)

--------------------------------------------------------------------------------
-- Pipeline error handling
--------------------------------------------------------------------------------

-- | Assert that an Either is Left and hand the diagnostic to a checker.
expectDiagnostic :: String -> Either Diagnostic a -> (Diagnostic -> Assertion) -> Assertion
expectDiagnostic _    (Left d)  check = check d
expectDiagnostic what (Right _) _     = assertFailure $ "expected a diagnostic for " ++ what

--------------------------------------------------------------------------------
-- Diagnostics rendering
--------------------------------------------------------------------------------

diagnosticsTests :: Test
diagnosticsTests = TestList
    [ TestLabel "renderDiagnostic with position and context" $ TestCase $
        assertEqual ""
            "g.pg:2:1: error: in rule 'Foo': bad thing"
            (renderDiagnostic "g.pg"
                (Diagnostic (Just (SourcePos 2 1)) (Just "in rule 'Foo'") "bad thing"))
    , TestLabel "renderDiagnostic with position, no context" $ TestCase $
        assertEqual ""
            "g.pg:2:1: error: bad thing"
            (renderDiagnostic "g.pg" (Diagnostic (Just (SourcePos 2 1)) Nothing "bad thing"))
    , TestLabel "renderDiagnostic without position or context" $ TestCase $
        assertEqual ""
            "g.pg: error: bad thing"
            (renderDiagnostic "g.pg" (Diagnostic Nothing Nothing "bad thing"))
    ]

--------------------------------------------------------------------------------
-- Pipeline error handling
--------------------------------------------------------------------------------

errorHandlingTests :: Test
errorHandlingTests = TestList
    [ TestLabel "grammar without rules is rejected" $ TestCase $
        expectDiagnostic "an empty grammar" (normalizeNoFillE "grammar 'Empty';") $ \d -> do
            assertBool ("unexpected message: " ++ diagMessage d) $
                "contains no rules" `isInfixOf` diagMessage d
                && "Empty" `isInfixOf` diagMessage d
            assertEqual "no position" Nothing (diagPos d)
    , TestLabel "empty input is a parse error" $ TestCase $
        expectDiagnostic "empty input" (parseGrammarSource "") $ \d -> do
            assertBool ("unexpected message: " ++ diagMessage d) $
                "end of input" `isInfixOf` diagMessage d
            assertEqual "position of end of input" (Just (SourcePos 1 1)) (diagPos d)
    , TestLabel "minimal valid grammar normalizes" $ TestCase $
        case normalizeGrammarSource "grammar 'Valid';\nRule = 'test' ;" of
            Right _ -> return ()
            Left d  -> assertFailure $ "valid grammar should normalize, got: " ++ show d
    , TestLabel "parse errors report the offending position" $ TestCase $
        -- ';' missing after the grammar declaration: the parser should point
        -- at the identifier 'Foo' on line 2
        expectDiagnostic "a missing ';'" (parseGrammarSource "grammar 'Test'\nFoo = bar;\n") $ \d -> do
            assertEqual "position of 'Foo'" (Just (SourcePos 2 1)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "identifier 'Foo'" `isInfixOf` diagMessage d
    , TestLabel "errors at end of input carry a position" $ TestCase $
        expectDiagnostic "a truncated rule" (parseGrammarSource "grammar 'Test';\nFoo =\n") $ \d -> do
            assertEqual "position of end of input" (Just (SourcePos 3 1)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "end of input" `isInfixOf` diagMessage d
    , TestLabel "a lexical first rule is rejected with an explanation" $ TestCase $
        expectDiagnostic "a lexical first rule"
            (normalizeGrammarSource "grammar 'Test';\nfoo = [a-z];\n") $ \d -> do
            assertBool ("unexpected message: " ++ diagMessage d) $
                "must be a syntax rule" `isInfixOf` diagMessage d
                && "foo" `isInfixOf` diagMessage d
            assertEqual "position of 'foo'" (Just (SourcePos 2 1)) (diagPos d)
    , TestLabel "normalization errors name the offending rule and position" $ TestCase $
        -- a lifted (,) clause mixed with other clauses is rejected; the error
        -- should point at rule 'Foo' on line 2
        expectDiagnostic "a mixed lifted clause"
            (normalizeGrammarSource "grammar 'Test';\nFoo = ,Bar Baz;\nBar = 'b';\nBaz = 'z';\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertEqual "position of the ',Bar' clause" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "lifted" `isInfixOf` diagMessage d
    , TestLabel "a lifted clause under * is rejected" $ TestCase $
        expectDiagnostic "a lifted clause under *"
            (normalizeGrammarSource "grammar 'X';\nFoo = ,Bar * ;\nBar = 'b';\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertEqual "position of the ',Bar *' clause" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "lifted" `isInfixOf` diagMessage d && "*" `isInfixOf` diagMessage d
    , TestLabel "repetition of a value-less item is rejected" $ TestCase $
        -- 'x' is matched but dropped (string literals normalize to ignored
        -- tokens), so 'x'* would collect nothing; it used to generate a
        -- parser that does not compile (issue #28)
        expectDiagnostic "a string literal under *"
            (normalizeGrammarSource "grammar 'I28';\nFoo = 'x'* ;\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertEqual "position of the repeated 'x'" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "produces no value" `isInfixOf` diagMessage d
    , TestLabel "repetition over a lexical rule is rejected" $ TestCase $
        -- a token payload has no grammar-owned data type to host the list
        -- element's splice constructor; it used to generate an invalid
        -- lowercase data declaration (issue #28)
        expectDiagnostic "a lexical rule under +"
            (normalizeGrammarSource "grammar 'I28';\nFoo = num+ ;\nnum = [0-9]+ ;\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertEqual "position of the repeated 'num'" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "lexical rule" `isInfixOf` diagMessage d && "num" `isInfixOf` diagMessage d
    , TestLabel "duplicate rule definitions are rejected" $ TestCase $
        -- the error points at the second definition and names the first
        expectDiagnostic "a duplicate rule"
            (normalizeGrammarSource "grammar 'Dup';\nFoo = 'a';\nFoo = 'b';\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertEqual "position of the second definition" (Just (SourcePos 3 1)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "defined more than once" `isInfixOf` diagMessage d
                && "line 2, column 1" `isInfixOf` diagMessage d
    , TestLabel "a reference to an unknown rule is rejected during generation" $ TestCase $
        expectDiagnostic "an unknown rule reference"
            (normalizeGrammarSource "grammar 'X';\nFoo = Nope;\n" >>= artifactsFor) $ \d -> do
            assertEqual "context names the referencing type" (Just "in type 'Foo'") (diagContext d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "unknown rule" `isInfixOf` diagMessage d && "Nope" `isInfixOf` diagMessage d
                -- the fallback names the type-named-rule convention that
                -- cover synthesis (issue #14) builds on
                && "Nope : SomeRule = ..." `isInfixOf` diagMessage d
    , TestLabel "a lexical value type is not referable, and the error says why" $ TestCase $
        -- 'Thing : num = ...' declares the token payload's Haskell type, not
        -- a syntax type: no cover rule can be synthesized from it (issue
        -- #14), and the diagnostic explains the convention instead of the
        -- bare "unknown rule"
        expectDiagnostic "a lexical value type reference"
            (normalizeGrammarSource "grammar 'X';\nFoo = Thing;\nThing : num = [0-9]+ ;\n"
                >>= artifactsFor) $ \d -> do
            assertEqual "context names the referencing type" (Just "in type 'Foo'") (diagContext d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "a type but not a rule" `isInfixOf` diagMessage d
                && "Thing : SomeRule = ..." `isInfixOf` diagMessage d
                && "lexical rule" `isInfixOf` diagMessage d
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
    , TestLabel "a referenced pure type group gets a synthesized cover rule" testTypeCoverSynthesis
    , TestLabel "a bare type as a list element resolves through the cover" testTypeCoverListElement
    , TestLabel "the start wrapper demands covers for unreferenced pure types" testTypeCoverWrapperDemand
    , TestLabel "no cover is synthesized for an unreferenced pure type (alias start)" testTypeCoverOnDemand
    , TestLabel "a precedence chain gets one splice alternative, at its bottom" testChainAttachment
    , TestLabel "splice attachment falls back to eligible rules when nothing covers" testAttachFallbackNoCover
    , TestLabel "shortcuts are recorded against the rule's type" testShortcuts
    , TestLabel "optional clauses desugar to an empty alternative" testOptionalDesugars
    , TestLabel "lexical rule type and conversion defaults" testLexicalRuleDefaults
    , TestLabel "start group wraps every public type in dummy tokens" testStartGroup
    , TestLabel "an alias start group gets no QQ start wrapper" testAliasStartGroup
    , TestLabel "fillConstructorNames fills every constructor and is idempotent" testFillConstructorNames
    ]

testStringLiterals :: Test
testStringLiterals = TestCase $ do
    let g = normalizeStringLiterals $ parseOrDie $ unlines
                [ "grammar 'Lit';"
                , "A = 'x' bb ;"
                , "B = 'x' A ;"
                , "bb = [b]+ ;"
                ]
        rules = grammarRules g
        tokenRules = [ r | r <- rules, ruleClause r == gpLit "x" ]
    -- both uses of 'x' share a single generated keyword rule
    case tokenRules of
        [tok] -> do
            assertEqual "keyword token type" (Just "Keyword") (ruleTypeName tok)
            let tokName = ruleName tok
                clauseOf n = ruleClause <$> find ((== n) . ruleName) rules
            assertEqual "literal in A replaced by ignored token"
                (Just $ gpSeq [gpIgnore (gpRef tokName), gpRef "bb"]) (clauseOf "A")
            assertEqual "literal in B replaced by ignored token"
                (Just $ gpSeq [gpIgnore (gpRef tokName), gpRef "A"]) (clauseOf "B")
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
    -- 'Expr' is a pure type group ("Top = Expr ;" references it but no rule
    -- carries the name), so rtk synthesizes the cover "Expr = ,Add | ,Mul"
    -- and the splice alternative sits only on the greedy attach point (Add,
    -- first by declaration); a $Expr:var splice climbs to Expr through the
    -- cover's unit production instead of every rule carrying a conflicting
    -- reduce item of its own
    let antiAltsOf name = [ alt | SyntaxRule rn (STAltOfSeq alts) <- allRules g, rn == name
                                , alt@(STSeq "Anti_Expr" _) <- alts ]
    assertEqual "Add (the attach point) accepts $Expr: splices"
        [STSeq "Anti_Expr" [SSId "qq_Expr"]] (antiAltsOf "Add")
    assertEqual "Mul reaches splices through the cover instead" [] (antiAltsOf "Mul")
    assertEqual "the synthesized cover carries no splice alternative" [] (antiAltsOf "Expr")

-- | Issue #14: a declared type may exist only through rule annotations (a
-- "pure type group" - no rule is named after the type). Referencing the
-- bare type name used to fail generation with "reference to unknown rule";
-- rtk now synthesizes the cover rule that authors write by hand, in lifted
-- form ("Thing = ,A | ,B"), so the type has a nonterminal and the lifted
-- alternatives pass the annotated rules' values through without adding an
-- AST constructor. The cover integrates with the splice attach machinery:
-- its alternatives are ordinary unit-production edges, so the splice
-- alternative lands on an annotated rule and a $Thing:var splice climbs to
-- Thing through the cover.
testTypeCoverSynthesis :: Test
testTypeCoverSynthesis = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'I14';"
                , "Start = Thing ;"
                , "Thing : A = aa ;"
                , "Thing : B = bb ;"
                , "aa = [a]+ ;"
                , "bb = [b]+ ;"
                ]
    -- the synthesized cover: one lifted alternative per annotated rule, in
    -- declaration order
    case [ alts | SyntaxRule "Thing" (STAltOfSeq alts) <- allRules g ] of
        [alts] -> assertEqual "lifted unit production per annotated rule"
            [STSeq "" [SSLifted "A"], STSeq "" [SSLifted "B"]] alts
        other -> assertFailure $ "expected one synthesized cover rule 'Thing', got: " ++ show other
    assertEqual "the cover is grouped under its type" ["Thing"]
        [ getSDataTypeName grp | grp <- getSyntaxRuleGroups g
                               , "Thing" `elem` map getSRuleName (getSRules grp) ]
    -- the covered group is an ordinary public data group: the start wrapper
    -- includes it, so it gets a top-level quoter like a hand-written type
    assertBool "the start wrapper covers Thing"
        ("Thing" `M.member` getRuleToStartInfo (getGrammarInfo g))
    -- splice integration: the alternative sits on the attach point A; the
    -- all-lifted cover itself carries none
    let antiAltsOf name = [ alt | SyntaxRule rn (STAltOfSeq alts) <- allRules g, rn == name
                                , alt@(STSeq "Anti_Thing" _) <- alts ]
    assertEqual "A (the attach point) accepts $Thing: splices"
        [STSeq "Anti_Thing" [SSId "qq_Thing"]] (antiAltsOf "A")
    assertEqual "B reaches splices through the cover instead" [] (antiAltsOf "B")
    assertEqual "the cover has no splice alternative of its own" [] (antiAltsOf "Thing")

-- | The original issue #14 shape: a list over the bare type name
-- ("Items = Thing* ~ ','"). The element resolves through the synthesized
-- cover - the list's element proxy is grouped under the type and lifts the
-- cover rule as its real-element alternative.
testTypeCoverListElement :: Test
testTypeCoverListElement = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'I14';"
                , "Start = Items ;"
                , "Items = Thing* ~ ',' ;"
                , "Thing : Leaf = num ;"
                , "num = [0-9]+ ;"
                ]
    assertEqual "lifted cover over the single annotated rule"
        [[STSeq "" [SSLifted "Leaf"]]]
        [ alts | SyntaxRule "Thing" (STAltOfSeq alts) <- allRules g ]
    proxyName <- case [ r | r <- allRules g, getSRuleName r == "Items"
                          , STMany{} <- [getSClause r] ] of
        [SyntaxRule _ (STMany STStar (SSId proxy) (Just _))] -> return proxy
        other -> fail $ "expected Items to stay a star rule over a proxy, got: " ++ show other
    assertEqual "the list proxy is grouped under the covered type" ["Thing"]
        [ getSDataTypeName grp | grp <- getSyntaxRuleGroups g
                               , proxyName `elem` map getSRuleName (getSRules grp) ]
    case find ((== proxyName) . getSRuleName) (allRules g) of
        Just (SyntaxRule _ (STAltOfSeq [STSeq "Anti_Thing" [SSId "qq_Items"],
                                        STSeq "" [SSLifted lifted]])) ->
            assertEqual "the proxy's real element is the cover" "Thing" lifted
        other -> assertFailure $ "unexpected proxy rule shape: " ++ show other

-- | The QQ start wrapper references every public type by name, so a
-- data-start grammar demands a cover even for a pure type the author never
-- references; generation used to fail on the wrapper's dangling reference
-- alone. The covered type then gets a top-level quoter like any other.
testTypeCoverWrapperDemand :: Test
testTypeCoverWrapperDemand = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'I14';"
                , "Start = Item ;"
                , "Item = ident ;"
                , "Thing : Leaf = ident ;"
                , "ident = [a-z]+ ;"
                ]
    assertEqual "cover synthesized for the wrapper-demanded type"
        [[STSeq "" [SSLifted "Leaf"]]]
        [ alts | SyntaxRule "Thing" (STAltOfSeq alts) <- allRules g ]
    assertBool "the start wrapper covers Thing"
        ("Thing" `M.member` getRuleToStartInfo (getGrammarInfo g))

-- | Synthesis is demand-driven. An alias start group generates no QQ start
-- wrapper, so a pure type that nothing references demands no cover and the
-- grammar's output stays exactly as before - no unused-nonterminal noise.
-- This is the issue #14 retest's I14B reproducer, which already generated.
testTypeCoverOnDemand :: Test
testTypeCoverOnDemand = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'I14B';"
                , "Items = Item* ~ ',' ;"
                , "Thing : Item = num ;"
                , "num = [0-9]+ ;"
                ]
    assertEqual "no cover rule synthesized" []
        [ r | r <- allRules g, getSRuleName r == "Thing" ]
    assertEqual "alias start group keeps no wrapper entries"
        M.empty (getRuleToStartInfo (getGrammarInfo g))

-- | In a shared-type group whose rules form a unit-production chain
-- (java.pg's Expression hierarchy in miniature), only the bottom rule of
-- the chain receives the splice alternative; a splice climbs the unit
-- productions to whatever level its position demands. One alternative
-- means one reduce item for the splice token, so the parser has no
-- reduce/reduce conflict to resolve arbitrarily.
testChainAttachment :: Test
testChainAttachment = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Chain';"
                , "Top = Expr ';' ;"
                , "Expr : Expr = Add ;"
                , "Expr : Add = Mul | Add '+' Mul ;"
                , "Expr : Mul = aa | Mul '*' aa ;"
                , "aa = [a]+ ;"
                ]
        antiAltsOf name = [ alt | SyntaxRule rn (STAltOfSeq alts) <- allRules g, rn == name
                                , alt@(STSeq "Anti_Expr" _) <- alts ]
    assertEqual "Mul (the chain bottom) accepts $Expr: splices"
        [STSeq "Anti_Expr" [SSId "qq_Expr"]] (antiAltsOf "Mul")
    assertEqual "Add has no splice alternative of its own" [] (antiAltsOf "Add")
    assertEqual "Expr has no splice alternative of its own" [] (antiAltsOf "Expr")

-- | When no attach point can cover a demanded rule (here T is all-lifted,
-- so it cannot carry the alternative, and no unit production reaches it),
-- attachment falls back to every eligible rule of the group -- and, just as
-- important, normalization terminates instead of retrying a candidate that
-- covers nothing.
testAttachFallbackNoCover :: Test
testAttachFallbackNoCover = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'X';"
                , "S = T ;"
                , "T : T = ,Q ;"
                , "T : U = uu ;"
                , "Q = qq ;"
                , "uu = [u]+ ;"
                , "qq = [q]+ ;"
                ]
        antiAltsOf name = [ alt | SyntaxRule rn (STAltOfSeq alts) <- allRules g, rn == name
                                , alt@(STSeq "Anti_T" _) <- alts ]
    assertEqual "U accepts $T: splices" [STSeq "Anti_T" [SSId "qq_T"]] (antiAltsOf "U")
    assertEqual "the lifted rule carries no splice alternative" [] (antiAltsOf "T")

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
    -- a data start group: alias start groups get no wrapper at all
    -- (see testAliasStartGroup)
    let g = normalizeNoFill $ unlines
                [ "grammar 'Mini';"
                , "Top = Item ;"
                , "Item = ident ;"
                , "ident = [a-z]+ ;"
                ]
        info = getGrammarInfo g
        startInfo = getRuleToStartInfo info
    assertEqual "start rule is the first user rule" (Just "Top") (getStartRuleName info)
    assertEqual "every public type gets a start entry"
        ["Item", "Top"] (sort $ M.keys startInfo)
    -- the synthesized start rule is prepended to the start group and wraps
    -- each type between its dummy tokens (compared via show because STSeq
    -- has no Ord instance)
    case map getSRules (getSyntaxRuleGroups g) of
        (SyntaxRule "Top" (STAltOfSeq alts) : _) : _ -> assertEqual "wrapper alternatives"
            (sort [ show (STSeq "" [SSIgnore d, SSId t, SSIgnore d]) | (t, d) <- M.toList startInfo ])
            (sort (map show alts))
        other -> assertFailure $ "expected the synthesized start rule first, got: " ++ show other
    -- each dummy token is lexed as a keyword spelled like its own name
    mapM_ (\dummy -> case find ((== dummy) . getLRuleName) (getLexicalRules g) of
              Just LexicalRule{getLRuleDataType = t, getLClause = cl} -> do
                  assertEqual ("dummy token type for " ++ dummy) "Keyword" t
                  assertEqual ("dummy token spelling for " ++ dummy) (gpLit dummy) cl
              other -> assertFailure $ "missing dummy keyword " ++ dummy ++ ": " ++ show other)
          (M.elems startInfo)

-- | A grammar whose START rule is a repetition normalizes to a type-alias
-- start group (type Start = [Item]). The QQ start-wrapper machinery (dummy
-- tokens, wrapper alternatives, top-level quoters) must be skipped for such
-- a grammar: a wrapper alternative needs a constructor in the start group's
-- data declaration, and an alias has none - injecting it used to generate a
-- parser that types the start nonterminal both as a data and as a list
-- (issue #34). The element-level splice machinery survives untouched.
testAliasStartGroup :: Test
testAliasStartGroup = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'I34';"
                , "Start = Item* ;"
                , "Item = 'x' ;"
                ]
        info = getGrammarInfo g
    assertEqual "no start wrapper entries" M.empty (getRuleToStartInfo info)
    assertEqual "start rule name is still recorded" (Just "Start") (getStartRuleName info)
    assertEqual "no dummy keyword tokens" []
        [ n | LexicalRule{getLRuleName = n} <- getLexicalRules g, "_dummy_" `isInfixOf` n ]
    -- the start group is exactly the user's list rule over the element proxy
    case getSyntaxRuleGroups g of
        startGroup : _ -> case getSRules startGroup of
            [SyntaxRule "Start" (STMany STStar (SSId proxy) Nothing)] ->
                assertBool ("list proxy survives: " ++ proxy)
                    ("ListElem_Start" `isPrefixOf` proxy)
            other -> assertFailure $ "expected only the bare list rule, got: " ++ show other
        [] -> assertFailure "no rule groups"
    -- element splices stay wired: the element type keeps its anti rule and
    -- the $Start:var splice token is still lexed
    assertEqual "anti rule for the element type"
        [AntiRule "Item" "Item" "Anti_Item" True] (getAntiRules g)
    assertBool "qq_Start splice token survives"
        ("qq_Start" `elem` [ n | LexicalRule{getLRuleName = n} <- getLexicalRules g ])

testFillConstructorNames :: Test
testFillConstructorNames = TestCase $ do
    let g = listGrammar
        filled = fillConstructorNames g
    assertEqual "no unnamed constructors remain" [] (unnamedConstructors filled)
    assertBool "anti constructors survive filling"
        ("Anti_Item" `elem` [ c | STSeq c _ <- allSeqs filled ])
    assertEqual "filling is idempotent" filled (fillConstructorNames filled)

--------------------------------------------------------------------------------
-- Named constructors ("Label: ..." alternative labels)
--------------------------------------------------------------------------------

namedConstructorTests :: Test
namedConstructorTests = TestList
    [ TestLabel "a label names the alternative's constructor; unnamed ones keep generated names" testCtorLabelOverride
    , TestLabel "the named constructor reaches the generated artifacts" testCtorLabelArtifacts
    , TestLabel "a label inside a parenthesized group names the proxy's constructor" testCtorLabelInGroup
    , TestLabel "a labeled sole repetition gets a constructor instead of a list alias" testCtorLabelForcesConstructor
    , TestLabel "both front ends parse labels into the same ICtor shape" testCtorLabelFrontEndEquality
    , TestLabel "a lowercase label is rejected" $ TestCase $
        expectDiagnostic "a lowercase constructor name"
            (normalizeGrammarSource "grammar 'X';\nFoo = bad: aa ;\naa = [a]+ ;\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertEqual "position of the label 'bad'" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "must start with an uppercase letter" `isInfixOf` diagMessage d
                && "bad" `isInfixOf` diagMessage d
    , TestLabel "the reserved Ctr__ prefix is rejected" $ TestCase $
        expectDiagnostic "a Ctr__ constructor name"
            (normalizeGrammarSource "grammar 'X';\nFoo = Ctr__Foo__0: aa ;\naa = [a]+ ;\n") $ \d -> do
            assertEqual "position of the label" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "reserved prefix 'Ctr__'" `isInfixOf` diagMessage d
    , TestLabel "the reserved Anti_ prefix is rejected" $ TestCase $
        expectDiagnostic "an Anti_ constructor name"
            (normalizeGrammarSource "grammar 'X';\nFoo = Anti_Foo: aa ;\naa = [a]+ ;\n") $ \d -> do
            assertEqual "position of the label" (Just (SourcePos 2 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "reserved prefix 'Anti_'" `isInfixOf` diagMessage d
    , TestLabel "duplicate explicit names are rejected across the whole grammar" $ TestCase $
        -- the two rules even have different types: constructor names share
        -- one generated Haskell module, so the clash is global
        expectDiagnostic "a duplicate constructor name"
            (normalizeGrammarSource
                "grammar 'X';\nFoo = Mk: aa ;\nBar = Mk: bb ;\naa = [a]+ ;\nbb = [b]+ ;\n") $ \d -> do
            assertEqual "context names the second rule" (Just "in rule 'Bar'") (diagContext d)
            assertEqual "position of the second label" (Just (SourcePos 3 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "already used in rule 'Foo'" `isInfixOf` diagMessage d
                && "line 2, column 7" `isInfixOf` diagMessage d
    , TestLabel "duplicate explicit names within one rule are rejected" $ TestCase $
        expectDiagnostic "a duplicate constructor name in one rule"
            (normalizeGrammarSource "grammar 'X';\nFoo = Mk: aa | Mk: bb ;\naa = [a]+ ;\nbb = [b]+ ;\n") $ \d ->
            assertBool ("unexpected message: " ++ diagMessage d) $
                "already used in rule 'Foo'" `isInfixOf` diagMessage d
    , TestLabel "a label in a lexical rule is rejected" $ TestCase $
        expectDiagnostic "a label in a lexical rule"
            (normalizeGrammarSource "grammar 'X';\nFoo = tok ;\ntok = Mk: [a]+ ;\n") $ \d -> do
            assertEqual "context names the lexical rule" (Just "in rule 'tok'") (diagContext d)
            assertEqual "position of the label 'Mk'" (Just (SourcePos 3 7)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "lexical rule" `isInfixOf` diagMessage d
    , TestLabel "a label on a lifted alternative is rejected" $ TestCase $
        -- a lifted alternative passes a value through and produces no
        -- constructor; silently dropping the name would be worse
        expectDiagnostic "a label on a lifted alternative"
            (normalizeGrammarSource "grammar 'X';\nFoo = Mk: ,Bar | aa ;\nBar = bb ;\naa = [a]+ ;\nbb = [b]+ ;\n") $ \d -> do
            assertEqual "context names the rule" (Just "in rule 'Foo'") (diagContext d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "produces no constructor" `isInfixOf` diagMessage d
                && "Mk" `isInfixOf` diagMessage d
    ]

-- | The user's name wins for the labeled alternatives; the unlabeled one is
-- still filled with a generated positional name, and the splice (Anti_)
-- machinery is unaffected.
testCtorLabelOverride :: Test
testCtorLabelOverride = TestCase $
    case normalizeGrammarSource namedExprGrammar of
        Left d  -> assertFailure $ "normalization failed: " ++ show d
        Right g -> assertEqual "constructors of the Expr group"
            (Just ["Anti_Expr", "Add", "Neg", "Ctr__Expr__0"])
            (lookupCtors "Expr" g)
  where
    lookupCtors t g = listToMaybe
        [ [ c | SyntaxRule _ (STAltOfSeq alts) <- getSRules grp, STSeq c _ <- alts ]
        | grp <- getSyntaxRuleGroups g, getSDataTypeName grp == t ]

namedExprGrammar :: String
namedExprGrammar = unlines
    [ "grammar 'Named';"
    , "Top = Expr ;"
    , "Expr = Add: Term '+' Term"
    , "     | Neg: '-' Term"
    , "     | Term ;"
    , "Term = num ;"
    , "Int: num = [0-9]+ ;"
    ]

-- | End to end: the names appear in the generated parser's data declaration
-- and semantic actions, with the leading RtkPos field like any generated
-- constructor.
testCtorLabelArtifacts :: Test
testCtorLabelArtifacts = TestCase $
    case normalizeGrammarSource namedExprGrammar >>= artifactsFor of
        Left d -> assertFailure $ "generation failed: " ++ show d
        Right artifacts -> case lookup "NamedParser.y" artifacts of
            Nothing -> assertFailure "no NamedParser.y artifact generated"
            Just y -> do
                assertBool "data declaration carries the named constructors"
                    ("Add RtkPos Term Term" `isInfixOf` y && "Neg RtkPos Term" `isInfixOf` y)
                assertBool "the unnamed alternative keeps a generated name"
                    ("Ctr__Expr__0 RtkPos Term" `isInfixOf` y)
                assertBool "semantic actions build the named constructor"
                    ("{ Add (rtkPosOf $1) $1 $3 }" `isInfixOf` y)
                assertBool "the named constructors have position projections"
                    ("rtkPosOf (Add p _ _) = p" `isInfixOf` y)

-- | A label inside a parenthesized group names the constructor of the
-- anonymous rule the group is extracted into - here under a repetition, so
-- the label also flows through the list-element proxy machinery.
testCtorLabelInGroup :: Test
testCtorLabelInGroup = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Grp';"
                , "S = (Pair: aa bb)+ ;"
                , "aa = [a]+ ;"
                , "bb = [b]+ ;"
                ]
    assertEqual "the extracted group's alternative carries the user's name"
        [STSeq "Pair" [SSId "aa", SSId "bb"]]
        [ alt | SyntaxRule rn (STAltOfSeq alts) <- allRules g
              , "Rule_" `isPrefixOf` rn, alt@(STSeq "Pair" _) <- alts ]

-- | A label always produces a constructor: a sole "L: Item*" alternative
-- yields a data declaration whose constructor wraps the (proxied) list,
-- where the unlabeled spelling would have made the rule a bare list alias.
testCtorLabelForcesConstructor :: Test
testCtorLabelForcesConstructor = TestCase $ do
    let g = normalizeNoFill $ unlines
                [ "grammar 'Lst';"
                , "Top = S ;"
                , "S = Wrap: Item* ;"
                , "Item = aa ;"
                , "aa = [a]+ ;"
                ]
    case [ alts | SyntaxRule "S" (STAltOfSeq alts) <- allRules g ] of
        [alts] -> assertBool ("expected a Wrap constructor over a proxy, got: " ++ show alts) $
            not $ null [ () | STSeq "Wrap" [SSId proxy] <- alts, "Rule_" `isPrefixOf` proxy ]
        other -> assertFailure $ "expected S to be a data group, got: " ++ show other

-- | Both front ends must represent labels identically: as a 'GP.Labeled'
-- node wrapping the alternative's sequence. Checked structurally against the
-- expected shape AND between the two front ends, for a plain labeled
-- alternative, a labeled group as a whole alternative, and a labeled group
-- in element position (parentheses are pure grouping, so the label node is
-- the only trace of the group). The label 'Mk_1' also pins identifier
-- lexing parity: grammar.pg's id rule allows underscores, which the
-- reference lexer used to reject.
testCtorLabelFrontEndEquality :: Test
testCtorLabelFrontEndEquality = TestCase $ do
    let src = unlines
            [ "grammar 'Eq';"
            , "S = Mk_1: aa bb | cc ;"
            , "T = (L: aa) ;"
            , "U = aa (M: bb) ;"
            , "aa = [a]+ ;"
            , "bb = [b]+ ;"
            , "cc = [c]+ ;"
            ]
    case (parseGrammarSource src, parseGrammarSourceGenerated src) of
        (Right h, Right g) -> do
            assertEqual "hand-written vs generated front end" h g
            assertEqual "hand-written vs generated front end (positions)"
                (positionsOf h) (positionsOf g)
            let clauseOf n = ruleClause <$> find ((== n) . ruleName) (grammarRules h)
            assertEqual "labeled alternative shape"
                (Just (gpAlt [ gpLabeled "Mk_1" (gpSeq [gpRef "aa", gpRef "bb"])
                             , gpRef "cc" ]))
                (clauseOf "S")
            assertEqual "labeled group as the whole alternative"
                (Just (gpLabeled "L" (gpRef "aa")))
                (clauseOf "T")
            assertEqual "labeled group in element position"
                (Just (gpSeq [gpRef "aa", gpLabeled "M" (gpRef "bb")]))
                (clauseOf "U")
        (Left d, _) -> assertFailure $ "hand-written front end failed: " ++ show d
        (_, Left d) -> assertFailure $ "generated front end failed: " ++ show d

--------------------------------------------------------------------------------
-- Self-hosted front end (the lexer/parser RTK generated from grammar.pg)
--------------------------------------------------------------------------------

selfHostedFrontEndTests :: Test
selfHostedFrontEndTests = TestList
    [ TestLabel "parse errors carry a structured position" $ TestCase $
        -- ';' missing after the grammar declaration. The generated parser
        -- encodes the position as "LINE:COL:message" and parseWithGenerated
        -- splits it back out, so the diagnostic points at the identifier
        -- 'Foo' on line 2 exactly like the hand-written front end's does.
        expectDiagnostic "a missing ';'"
            (parseGrammarSourceGenerated "grammar 'Test'\nFoo = bar;\n") $ \d -> do
            assertEqual "position of 'Foo'" (Just (SourcePos 2 1)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "unexpected id" `isInfixOf` diagMessage d
    , TestLabel "parse errors point where the reference front end points" $ TestCase $
        -- The wording differs (the generated parser renders tokens
        -- generically, the reference parser knows the grammar language's
        -- token names) but the position must be the same.
        case ( parseGrammarSource          "grammar 'Test'\nFoo = bar;\n"
             , parseGrammarSourceGenerated "grammar 'Test'\nFoo = bar;\n" ) of
            (Left dh, Left dg) -> assertEqual "same position" (diagPos dh) (diagPos dg)
            _ -> assertFailure "both front ends should reject the missing ';'"
    , TestLabel "lexical errors are identical to the reference front end's" $ TestCase $
        -- Both lexers encode "LINE:COL:lexical error. Following chars: ...",
        -- so the rendered FILE:LINE:COL: error line is the same character
        -- for character.
        case ( parseGrammarSource          "grammar 'Test';\nA = % ;\n"
             , parseGrammarSourceGenerated "grammar 'Test';\nA = % ;\n" ) of
            (Left dh, Left dg) -> do
                assertEqual "identical diagnostics" dh dg
                assertEqual "position of '%'" (Just (SourcePos 2 5)) (diagPos dg)
            _ -> assertFailure "both front ends should reject the '%'"
    , TestLabel "rule positions are captured: duplicate rules get a structured diagnostic" $ TestCase $
        -- The generated AST carries the position of every rule's first
        -- token, and normalization reads it directly, so a normalization
        -- diagnostic under the default front end points at the offending
        -- line and column exactly like the hand-written front end.
        expectDiagnostic "a duplicate rule"
            (parseGrammarSourceGenerated "grammar 'Dup';\nFoo = 'a';\nFoo = 'b';\n"
                >>= normalizeParsedGrammar) $ \d -> do
            assertEqual "position of the second definition" (Just (SourcePos 3 1)) (diagPos d)
            assertBool ("unexpected message: " ++ diagMessage d) $
                "defined more than once" `isInfixOf` diagMessage d
                && "line 2, column 1" `isInfixOf` diagMessage d
    ]

--------------------------------------------------------------------------------
-- The compiled-in generated quasi-quoter (GrammarQQ)
--------------------------------------------------------------------------------

-- | Smoke test for the snapshot's quasi-quoter, compiled into rtk beside
-- GrammarLexer/GrammarParser: the quotes below are parsed by the generated
-- front end at rtk's OWN compile time, and Anti_* splices work in both
-- expression and pattern context. The quotes produce the generated AST types
-- ('GrammarParser.Clause' here) - the same types the pipeline computes over
-- since the 8c migration, so quoted fragments can feed normalization-level
-- rewrites (task 8d in docs/qq-grammar-rewrites-plan.md).
grammarQQTests :: Test
grammarQQTests = TestList
    [ TestLabel "an expression quote builds a clause and splices $vars" $ TestCase $ do
        -- $cl resolves via grammar.pg's @shortcuts(cl) to a Clause
        -- metavariable; in expression context the Anti_Clause splice inserts
        -- the local 'cl' into the quoted star repetition
        let cl = [clause| Name |] :: Clause
        assertEqual "spliced clause equals the directly quoted one"
            [clause| Name * |] [clause| $cl * |]
    , TestLabel "a pattern quote matches modulo positions and binds splices" $ TestCase $
        -- pattern and scrutinee come from different quote bodies, so their
        -- positions differ; patterns wildcard every RtkPos field, and the
        -- metavariable binds the repeated clause through the Anti_Clause
        -- splice
        assertEqual "the inner clause is bound"
            (Just [clause| Name |]) (starInner [clause| Name * |])
    , TestLabel "a pattern quote still discriminates clause shapes" $ TestCase $
        assertEqual "a plus clause does not match the star pattern"
            Nothing (starInner [clause| Name + |])
    , TestLabel "the quoted language's own literals lex inside quotes" $ TestCase $
        -- Name '*' is a sequence (a rule reference, then a string literal),
        -- distinct from the star repetition Name *
        assertBool "sequence with a '*' literal differs from the star clause"
            ([clause| Name '*' |] /= [clause| Name * |])
    ]

-- | A matcher defined by a quasi-quoted pattern. The scrutinee stays an
-- opaque argument: applied to a quoted clause directly, GHC sees the
-- expanded constructors on both sides, proves each match's outcome at
-- compile time and rejects the would-be-exercised alternative as redundant
-- under -Werror.
starInner :: Clause -> Maybe Clause
starInner [clause| $cl2 * |] = Just cl2
starInner _                  = Nothing

-- | Both front ends must produce the same parsed grammar ('GP.Grammar') for
-- every grammar in the corpus, source positions included. Equality alone no
-- longer proves position agreement - 'GP.RtkPos' is equality-transparent -
-- so the positions of EVERY node are projected with 'positionsOf' and
-- compared explicitly (strictly stronger than the rule-position comparison
-- the retired InitialGrammar equality gave). This catches front-end bugs
-- with far better messages than a generated-artifact diff. Grammars pinned
-- in 'frontEndDivergentGrammars' are expected to differ (or be rejected);
-- the test fails once they agree, so the pin gets dropped.
astEqualityTestFor :: FilePath -> IO Test
astEqualityTestFor pgFile = do
    source <- readFileUtf8 pgFile
    let grammarKey = takeBaseName pgFile
        hand = parseGrammarSource source
        gen  = parseGrammarSourceGenerated source
    return $ TestLabel (grammarKey ++ " front-end AST equality") $ TestCase $
        case (lookup grammarKey frontEndDivergentGrammars, hand, gen) of
            (_, Left d, _) -> assertFailure $
                "hand-written front end failed on " ++ pgFile ++ ": " ++ show d
            (Nothing, Right h, Right g) -> do
                assertEqual "hand-written vs generated front end" h g
                assertEqual "hand-written vs generated front end (node positions)"
                    (positionsOf h) (positionsOf g)
            (Nothing, _, Left d) -> assertFailure $
                "generated front end failed on " ++ pgFile ++ ": " ++ show d
            (Just _, _, Left _) -> return () -- rejected: still divergent
            (Just reason, Right h, Right g) ->
                when (h == g) $ assertFailure $
                    "the front ends now agree on this grammar (pinned because: " ++ reason
                    ++ ");\ndrop it from frontEndDivergentGrammars in test/TestSupport.hs"

-- | Every node position in the parsed grammar, in traversal order, as
-- (line, column). 'GP.RtkPos' equality is transparent by design (so ASTs
-- from different sources compare equal), which makes this explicit
-- projection the only way to assert position agreement.
positionsOf :: GP.Grammar -> [(Int, Int)]
positionsOf = everything (++) ([] `mkQ` project)
  where project (GP.RtkPos (GL.AlexPn _ line col)) = [(line, col)]

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
                `removeAll` maybeToList (getStartRuleName info)
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
            -- An alias start group (the grammar's first rule is a
            -- repetition, e.g. debug-test's "Program = Statement *") gets
            -- no start wrapper and no start info: there is no data
            -- declaration for the wrapper constructors to extend
            -- (see Normalize.addStartGroup, issue #34)
            Just startName
              | startGroupIsAlias -> do
                    assertEqual "only the original rule for an alias start group"
                        1 (length [ () | r <- allRules g, getSRuleName r == startName ])
                    assertEqual "no start info for an alias start group"
                        M.empty (getRuleToStartInfo info)
              | otherwise -> do
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
    startGroupIsAlias = case getSyntaxRuleGroups g of
        grp : _ -> any (notAltOfSeq . getSClause) (getSRules grp)
        []      -> False
      where notAltOfSeq STAltOfSeq{} = False
            notAltOfSeq _            = True
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
duplicates xs = [ x | x : _ : _ <- group (sort xs) ]

removeAll :: Eq a => [a] -> [a] -> [a]
removeAll xs banned = [ x | x <- xs, x `notElem` banned ]
