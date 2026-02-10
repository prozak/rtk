# RTK Codebase Review

## Executive Summary

RTK is a well-structured Haskell project that generates lexers, parsers, and
quasi-quoters from grammar specifications. The architecture is sound: a clear
pipeline from lexing through normalization to code generation, with a
self-hosting bootstrap capability. However, the codebase has several categories
of issues that affect reliability, maintainability, and correctness.

**Critical issues**: 6 unsafe `fromJust` calls that will crash on malformed input
**High issues**: Partial functions, incomplete implementations, confusing control flow
**Medium issues**: Code duplication, overly complex generic programming, dead code
**Low issues**: Style inconsistencies, redundant code

---

## 1. CRITICAL: Unsafe Partial Functions (`fromJust`)

The most pressing issue across the codebase is the pervasive use of `fromJust`
without defensive checks. Each of these is a potential runtime crash with an
unhelpful error message.

### Normalize.hs

**Line 296** - Map lookup without safety:
```haskell
dummy = SSIgnore (fromJust (M.lookup typeName ruleToStartInfo))
```
If `typeName` is not in `ruleToStartInfo`, this crashes with a generic
`Maybe.fromJust: Nothing` error, giving no indication of which rule caused
the failure.

**Line 305** - Optional field treated as required:
```haskell
qqRule = SyntaxRule (fromJust (getStartRuleName info)) $ STAltOfSeq rulesClauses
```
`getStartRuleName` returns `Maybe String`. This crashes when no start rule is
defined in the grammar.

**Line 322** - Assumption about prior state mutation:
```haskell
firstRuleGroupRules = fromJust $ M.lookup firstID nrs
```
Assumes `firstID` was successfully added during `doNM`, but doesn't verify it.

### GenQ.hs

**Line 110** - Start rule name in quasi-quoter prototype:
```haskell
qqFunProtoGen typ = [str|?(qqFunName typ) :: ... -> (?(fromJust $ getStartRuleName info) -> a) ...|]
```

**Line 135** - Rule-to-start-info lookup:
```haskell
dummy = "\"" ++ (fromJust $ M.lookup typeName $ getRuleToStartInfo info) ++ "\""
```

**Line 137** - Type name to constructor lookup:
```haskell
dataConstructor = fromJust $ M.lookup typeName typeNameToConstructor
```

### Recommendation

Replace every `fromJust` with explicit `case` or `fromMaybe` with a
descriptive error message:
```haskell
-- Before:
fromJust (M.lookup typeName ruleToStartInfo)

-- After:
case M.lookup typeName ruleToStartInfo of
    Just v  -> v
    Nothing -> error $ "Normalize: rule '" ++ typeName ++ "' not found in ruleToStartInfo"
```

---

## 2. HIGH: Error Handling via `error` for Control Flow

### main.hs

The `exitAfterDebug` function (line 206-210) uses `error` for normal control flow:
```haskell
exitAfterDebug :: IO ()
exitAfterDebug = do
    putStrLn "Stopped after requested debug stage."
    error "Debug stage exit"
```

This is called from lines 64, 77, 90, 103, and 116. Using `error` produces a
stack trace and a non-zero exit code that suggests a crash rather than a
successful debug-and-stop. The function should use `System.Exit.exitSuccess`
instead.

Similarly, line 40:
```haskell
error "Generated parser mode not yet available"
```
Should use `exitFailure` with a proper error message to stderr.

### Confusing Boolean Condition (main.hs:186)

```haskell
when (not (validateGrammar opts) ||
      not (validateGrammar opts &&
           not (any id [debugParserSpec opts, debugLexerSpec opts, debugQQSpec opts]))) $ do
```

This condition is nearly impossible to read. Applying De Morgan's laws:
- `not A || not (A && not B)` simplifies to `not A || not A || B`, which is
  `not A || B`.

The intended logic appears to be: "write output files unless we're in
validate-only mode without any debug spec flags." This should be refactored to
a named boolean with a comment explaining the intent.

---

## 3. HIGH: Incomplete Implementations

### GenY.hs - TODO comments marking incomplete code

**Line 115**:
```haskell
-- TODO: check whether reverse is needed (monad again) (switch to left recursion)
```

**Line 118**:
```haskell
-- TODO: no lifted yet, need monad with rules map here
genSimpleClause (SSLifted idName) = text idName
```

Lifted rules are silently treated as plain identifiers, producing incorrect
parser output without any warning to the user.

### GenAST.hs - Unimplemented features crash at runtime

**Line 65**:
```haskell
genSimpleItem _ (SSLifted _) = error "lifted rules are not yet implemented"
```

### StrQuote.hs - Stub implementations

**Line 108**:
```haskell
quotePat  = litP . stringL        -- patterns as string literals (not real patterns)
quoteType = \_ -> return ListT    -- hardcoded to ListT (wrong)
quoteDec  = \_ -> return []       -- always returns empty (no-op)
```

These stub implementations could produce confusing behavior if someone tries to
use `str` as a pattern or type quasi-quoter.

### PrintGrammar.hs - Entirely commented out

The entire module body is commented out (lines 4-42). It either needs to be
updated to work with the current grammar data structures or removed entirely.

---

## 4. HIGH: Unsafe String Operations

### GenQ.hs:45 and GenX.hs

```haskell
varName = init $ tail match
```

`tail` and `init` are partial functions that crash on empty strings. If `match`
is `""` or a single character, this will throw an exception. The code should
validate string length first or use safe alternatives like `Data.List.stripPrefix`.

### Lexer.x:40-45 - Inefficient string trimming

```haskell
simple1 $ StrLit . (reverse.drop 1.reverse.drop 1)
```

This reverses the string twice to remove the first and last characters. While
correct, it's O(n) twice unnecessarily. Using `init . tail` or
`take (n-2) . drop 1` would be clearer and equally efficient (both are O(n) on
lists, but `init . tail` is more idiomatic).

The triple-quoted version is worse:
```haskell
reverse.(drop 3).reverse.(drop 3)
```

---

## 5. MEDIUM: Code Duplication

### GenX.hs:115-139 - Duplicate escaping functions

`backquoteStr` and `backquoteStrInBrackets` share nearly identical structure
but differ only in which characters need escaping. They should be refactored
into a single function parameterized by the escape character set:

```haskell
backquoteWith :: [Char] -> String -> String
backquoteWith specialChars s = concatMap escape s
  where
    escape c | c `elem` specialChars = ['\\', c]
             | otherwise             = [c]
```

### Debug.hs - Duplicated dependency extraction

`buildDependencyMap` (line 316) and `checkUndefinedReferences` (line 367)
both implement nearly identical rule-traversal logic (`extractFromRule`,
`extractFromSimple`, `extractFromSeq`). This traversal should be factored into
a shared utility in Grammar.hs.

---

## 6. MEDIUM: Overly Complex Generic Programming

### Normalize.hs:257

```haskell
let grammar0 = everywhereBut (False `mkQ` (isLexicalRule . getIRuleName))
                              (mkT removeOpts) grammar
```

This uses SYB (Scrap Your Boilerplate) generic traversal with double-negative
logic:
- `False` is the default query result
- `True` means "stop here" (it's a lexical rule)
- `everywhereBut` applies the transformation where the query is `False`

This is difficult to understand and debug. Consider replacing with explicit
pattern matching on the grammar structure, which would be longer but
significantly more readable and maintainable.

### Normalize.hs:349

```haskell
let (dat1, _) = runState (everywhereM (mkM (fillConstructorName n)) dat)
                         (FillNameState 0 n)
```

Generic monadic traversal makes it hard to verify which nodes are visited. An
explicit traversal function on the known grammar ADT would be more transparent.

---

## 7. MEDIUM: Fragile Record Destructuring

### Normalize.hs:320-321

```haskell
(_, NormalizationState nrs nls counter antiRules shortcuts proxyRules _ _ _) =
    runState (doNM grammar) (NormalizationState M.empty [] 0 [] [] S.empty M.empty M.empty ruleTypeMap)
```

This positional pattern match on a 9+ field record breaks whenever a field is
added or reordered. Use record pattern syntax instead:

```haskell
let st = execState (doNM grammar) initialState
    nrs = _normSRules st
    nls = _normLRules st
    ...
```

---

## 8. MEDIUM: Silent Failures in StrQuote.hs

### Line 84 - Unsupported expressions become strings

```haskell
convertToTHHelper e = stringE ("<unsupported expression: " ++ show e ++ ">")
```

When an expression can't be converted to Template Haskell, it silently becomes
a string literal `"<unsupported expression: ...>"` in the output. This will
compile without error but produce incorrect runtime behavior. It should be a
compile-time error.

### Line 90 - Parse failures become strings

```haskell
ParseFailed loc errMsg -> stringE ("<" ++ errMsg ++ ":" ++ show loc ++ ">")
```

Same issue: parse failures produce string output instead of compile-time errors.

---

## 9. LOW: Style and Minor Issues

### Redundant `return ()` in Normalize.hs

Lines 55, 61, 66, 71 all have patterns like:
```haskell
saveProxyRuleName ruleName = do
    proxyRuleNames %= S.insert ruleName
    return ()
```

The `%=` operator already returns `()`, so the explicit `return ()` is
redundant. Remove for cleaner code.

### Explicit `Prelude.id` qualification (Normalize.hs:168)

```haskell
let dtName = (maybe rn Prelude.id dtn)
```

Qualifying `id` with `Prelude` is unusual and suggests a naming conflict. If
there's a conflict, consider renaming the conflicting identifier. If not,
remove the qualification.

### Test file reimplements standard library function

EmptyGrammar_Test.hs (lines 73-81) implements `isInfixOf` and `isPrefixOf`
from scratch instead of importing from `Data.List`. This adds unnecessary code
and potential for divergent behavior.

### Debug.hs - Placeholder format handlers

**Line 102**:
```haskell
formatContent FormatJSON _ = "{}"  -- Placeholder
formatContent FormatTree s = s     -- No tree formatting
```

These format options are advertised in the CLI help but don't actually work.
They should either be implemented or removed from the available options.

### Lexer.x:66 - Comment depth counter can go negative

The multiline comment handler decrements `commentDepth` without checking if
it's already at zero. A mismatched `*/` will make the counter go negative,
potentially causing subsequent `/*` comments to not be handled correctly.

---

## 10. Architecture & Design Observations

### Strengths

1. **Clear pipeline architecture**: The flow from lexing -> parsing ->
   normalization -> code generation is well-structured and easy to follow.
2. **Self-hosting capability**: The bootstrap comparison test is an excellent
   correctness validation mechanism.
3. **Comprehensive debug infrastructure**: The debug options system is thorough
   and well-designed with good CLI ergonomics.
4. **Good use of Haskell's type system**: The grammar ADT captures the domain
   well, and the separation between `InitialGrammar` and `NormalGrammar` is
   sound.

### Areas for Improvement

1. **Error reporting**: The project needs a consistent error-reporting strategy.
   Currently it's a mix of `error` (crashes), silent fallbacks (empty strings),
   and proper error messages. Consider using `Either String a` or a custom error
   type throughout the pipeline.
2. **Test coverage**: Only 2 unit test files exist (StrQuote_Test, EmptyGrammar_Test).
   The code generators (GenAST, GenQ, GenX, GenY) and normalizer have no unit
   tests. These are the most complex modules and would benefit most from testing.
3. **String-based code generation**: The code generators mix Pretty printing
   (`Text.PrettyPrint.Doc`) with raw string concatenation and quasi-quotation.
   Standardizing on one approach would improve maintainability.
4. **Dependency on SYB (Scrap Your Boilerplate)**: The generic programming in
   Normalize.hs is powerful but opaque. For a project of this size, explicit
   pattern matching would be more maintainable.
5. **No property-based testing**: Grammar normalization is an ideal candidate for
   QuickCheck property tests (e.g., normalization is idempotent, generated code
   compiles, round-trip properties).

---

## Summary of Issues by Severity

| Severity | Count | Category |
|----------|-------|----------|
| CRITICAL | 6 | Unsafe `fromJust` calls that crash on malformed input |
| HIGH | 8 | `error` for control flow, incomplete features, unsafe string ops |
| MEDIUM | 7 | Code duplication, complex generic programming, silent failures |
| LOW | 5 | Style issues, dead code, missing stdlib imports |

### Priority Fix Order

1. Replace all `fromJust` calls with proper error handling
2. Replace `error` with `exitSuccess`/`exitFailure` in main.hs
3. Simplify the file-write condition in main.hs:186
4. Add bounds checking to `init`/`tail` calls on strings
5. Address silent failures in StrQuote.hs
6. Add unit tests for code generators
7. Refactor duplicated code in GenX.hs and Debug.hs
8. Remove or update dead code (PrintGrammar.hs)
