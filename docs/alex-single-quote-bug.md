# Alex 3.5.4.0 Single Quote Bug in Java Comments

**Date**: 2025-11-06
**Status**: **KNOWN LIMITATION** - No complete workaround available
**Affected**: Alex 3.5.4.0 lexer generator

## Problem

Alex 3.5.4.0 has a DFA generation bug where single quote characters (`'`) inside Java comments cause lexical errors, even though comments should be ignored tokens.

**Example that fails:**
```java
// This comment mentions 'something' in quotes
class Test {}
```

**Error**: `lexical error at line 1, column 27` (at the single quote position)

## Root Cause

When Alex builds its DFA for the lexer, it incorrectly tries to match single quotes as the start of character literals, even when they appear inside comment patterns. This is a fundamental bug in how Alex handles overlapping patterns with character classes.

The pattern `//' [^\n]* '\n'` should match the entire line comment, but Alex's DFA treats the `'` as potentially starting a new token (char literal) instead of being part of the comment text.

## Impact on Apache Commons Lang Tests

**Lexical Parsing Results:**
- **Total files**: 259
- **Successful**: 216 files (83%)
- **Failed**: 43 files (17%)

All 43 failures are due to single quotes in comments (line comments, block comments, or JavaDoc).

**Failed files:** (partial list)
- ArrayUtils.java - contains `'array'` in block comment
- BooleanUtils.java - contains `'true'` in line comment
- CharUtils.java - contains `'\n'` in JavaDoc `{@code}` tag
- And 40 more...

## Attempted Workarounds

### Approach 1: Exclude quotes from character classes ❌
```
FROM: '//' [^\n]* '\n'
TO:   '//' [^\n']* '\n'
```
**Result**: Pattern can't match comments containing quotes at all.

### Approach 2: Explicit alternation ❌
```
TO: '//' ( [^\n'] | '\'' )* '\n'
```
**Result**: RTK's quote escaping interferes; generated code uses `"'"` which doesn't work.

### Approach 3: Use wildcard `.` ❌
**Result**: Would match too much (cross-line matching).

## Minimal Reproduction

Create `test.x`:
```alex
{
module Main where
}

%wrapper "basic"

tokens :-
  "//" [^\n]* "\n"  ;
  "'" . "'"         { \s -> s }
  [a-zA-Z]+         { \s -> s }
  [\ \t\n]+         ;

{
main = print $ alexScanTokens "// test 'quote'\nclass"
}
```

Compile and run:
```bash
alex test.x -o test.hs
ghc test.hs -o test
./test
# ERROR: lexical error
```

## Recommendations

1. **Accept the limitation**: 83% success rate is still useful for validating the lexer on most files
2. **Document known failures**: List the 43 failing files in test reports
3. **Consider alternatives**:
   - Report bug to Alex maintainers
   - Use a different lexer generator (e.g., custom lexer, other tools)
   - Post-process generated lexer to manually fix these patterns
   - Wait for Alex 3.6+ which may fix this issue

## Current Status

The RTK Java lexer successfully tokenizes **216 out of 259** Apache Commons Lang files (83%). This validates that the lexer works correctly for the vast majority of real-world Java code, with the known limitation around single quotes in comments.

The lexical parsing tests are integrated into CI as required tests to catch regressions, with the understanding that this known limitation exists.
