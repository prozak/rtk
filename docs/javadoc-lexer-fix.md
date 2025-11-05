# JavaDoc Comment Lexer Fix

**Date**: 2025-11-04
**Status**: FIXED ✅

## Problem

JavaDoc comments with blank lines followed by `{@link Class#method()}` patterns failed to lex, causing 100% test failure on Apache Commons Lang (259 files).

**Failing pattern**:
```java
/**
 * First paragraph.
 *
 * See {@link Annotation#equals(Object)} for details.
 */
```

**Error**: `lexical error at line X, column Y` (at the `#` character)

## Root Cause

The Alex lexer pattern `([^\*] | [\*] [^/])*` was ambiguous when handling star-newline sequences:

```
/**
 *           ← works fine
 *           ← blank line: matches [\*] [^/] as "* " then "\n"
 * {@link A#b}  ← fails here: pattern already consumed the star, can't match " * {@link"
 */
```

The DFA couldn't correctly match ` *\n` followed by ` * {@link A#b}`.

## Solution

Split the `[\*] [^/]` alternation into explicit cases:

```
FROM: ([^\*] | [\*] [^/])*
TO:   ([\n] | [^\*\n] | [\*] [^/\n] | [\*] [\n])*
```

This explicitly handles:
1. `[\n]` - newlines alone
2. `[^\*\n]` - non-star, non-newline chars
3. `[\*] [^/\n]` - star + non-slash non-newline
4. `[\*] [\n]` - star + newline (critical for blank lines!)

## Implementation

**test-grammars/java.pg** (lines 410, 417):
```pg
# Fixed doccomment pattern
doccomment = '/**' ([\n] | [^*\n] | [*] [^/\n] | [*] [\n])* '*/';

# Fixed blockComment pattern (same fix)
Ignore: blockComment = '/*' [^*] ([\n] | [^*\n] | [*] [^/\n] | [*] [\n])* '*/' ;
```

**GenX.hs** (line 156) - also fixed spacing bug:
```haskell
-- BEFORE: translateClause sMacroIds (IStar cl Nothing) = translateClause sMacroIds cl <+> text "*"
-- AFTER:
translateClause sMacroIds (IStar cl Nothing) = translateClause sMacroIds cl <> text "*"
```

Changed `<+>` to `<>` to prevent space insertion before `*` repetition operator.

## Test Results

Created 5 regression tests, all passing:

✅ `test-blank-then-link.java` - Blank line + `{@link A#b}` (was failing)
✅ `test-minimal-hash.java` - Blank line + `#` (was failing)
✅ `test-minimal-fail.java` - Full Apache Commons Lang case (was failing)
✅ `test-link-tag.java` - `{@link A#b}` without blank line
✅ `test-just-hash.java` - Multi-line with `a#b`

All tests added to CI via `make test-all-java`.

## Impact

- **Before fix**: 0/259 Apache Commons Lang files parsed (100% failure)
- **After fix**: Lexer works correctly, remaining failures are parse errors for unsupported Java 8 features (lambdas, method references, etc.)

## Technical Details

The key insight: Alex's DFA generation handles explicit alternation cases better than combined character classes when dealing with sequences involving repetition. By splitting the `[\*] [^/]` pattern, we made the state transitions explicit:

- State A: See `*` → go to State B
- State B: See `/` → end comment
- State B: See `\n` → stay in State B (blank line handling!)
- State B: See other → go back to State A

This prevents the DFA from getting "stuck" after consuming `*\n`.
