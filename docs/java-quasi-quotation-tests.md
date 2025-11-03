# Java Quasi-Quotation Tests

## Overview

This document describes the Java quasi-quotation tests that were re-enabled after fixing the parser issues that previously caused them to be disabled (commit 98c9397).

## File Organization

The Java test suite is organized as follows:

- **`java-main.hs`**: Simple parser driver for testing Java file parsing. Takes a Java file as input and outputs the parsed AST.
- **`java-qq-test.hs`**: Dedicated quasi-quotation test file. Tests the Template Haskell quasi-quoters for constructing and pattern matching Java AST nodes.

This separation keeps the regular parsing driver clean and focused, while allowing quasi-quotation tests to be developed and debugged independently.

## Background

Quasi-quotations were originally disabled in commit 98c9397 due to:
1. Parser failures on multi-argument method calls
2. Parser failures on `return` keyword
3. Parser failures on method bodies with assignments

These issues have since been fixed in commit f39a9cc with the method body parsing fixes.

## Test File Location

**File**: `/home/user/rtk/test-grammars/java-qq-test.hs`

This file is separate from `java-main.hs`, which remains a simple parser driver for regular Java file parsing.

## Current Test Cases

The java-qq-test.hs file includes **26 comprehensive tests** covering quasi-quotation construction:

### Construction Tests (Tests 1-26)

Building AST nodes from Java syntax:

- **Basic expressions** (Tests 1-3): binary operators, method calls, literals
- **Basic statements** (Tests 4-5): return and assignment statements
- **Multi-argument method calls** (Tests 6-8): 2-arg, 3-arg, nested calls
- **Complex expressions** (Tests 9-14): operators, arrays, ternary, new, cast, strings
- **Blocks and control flow** (Tests 15-20): statement blocks, if/else, while, for
- **CompoundName, modifiers, literals** (Tests 21-26): qualified names, modifiers, literals

Example:
```haskell
let expr1 = [expression| x + y |]
let stmt1 = [statement| return; |]
let block1 = [statementBlock| { return x; } |]
```

### What's Not Supported

**Pattern Matching** and **Anti-Quotation (Splicing)** are not currently functional for the Java grammar:

- ❌ Pattern matching: `case expr of [expression| $e1 + $e2 |] -> ...` fails with parse errors
- ❌ Anti-quotation: `let e = [expression| x |] in [expression| $e + 1 |]` fails with parse errors

Only **construction** mode works: directly building AST nodes from Java syntax literals.

## Building and Running Tests

### Build the parser and QQ modules:
```bash
make build
```

This will generate:
- `test-out/JavaLexer.hs` - Lexer from java.pg
- `test-out/JavaParser.hs` - Parser from java.pg
- `test-out/JavaQQ.hs` - Quasi-quoter module from java.pg

### Run the quasi-quotation tests:
```bash
make test-java-qq
```

This will:
1. Copy `java-qq-test.hs` to test-out
2. Compile it with the generated Java parser modules
3. Run the quasi-quotation tests

### Run regular Java file parsing tests:
```bash
make test-java-minimal
```

Or run all Java tests:
```bash
make test-all-java
```

## Potential Issues and Debugging

### Issue 1: QuasiQuoter Not Found

**Symptom**: Error like `Not in scope: 'expression'`

**Cause**: The quasi-quoter name doesn't match the grammar rule name.

**Fix**: Check the java.pg grammar file for the exact rule name. Quasi-quoters use lowercase versions of rule names.

### Issue 2: Parse Errors in Quasi-Quotations

**Symptom**: Compile-time parse errors when using quasi-quotations

**Cause**: The syntax inside the quasi-quotation doesn't match the grammar.

**Debug approach**:
1. Check if the same code parses as a standalone Java file
2. Look at the grammar rule definition in java.pg
3. Ensure the quasi-quotation includes complete syntax (e.g., semicolons for statements)

### Issue 3: Multi-Argument Method Calls

**Previous issue**: `[expression| obj.method(arg1, arg2) |]` failed

**Status**: Should be fixed with method body parsing improvements

**Test approach**:
1. Start with single-argument calls
2. Gradually add multi-argument calls
3. Test with different argument types (literals, variables, expressions)

### Issue 4: Type Mismatches

**Symptom**: Haskell type errors when using quasi-quotation results

**Cause**: The quasi-quoter returns the AST type corresponding to the rule name

**Fix**: Ensure variable types match. For example:
- `[expression| ... |]` returns type `Expression`
- `[statement| ... |]` returns type `Statement`

## Quasi-Quotation Features

### Supported: Construction
Build AST nodes directly from Java syntax:
```haskell
let expr = [expression| x + y * z |]
let stmt = [statement| return x; |]
let block = [statementBlock| { x = 1; return x; } |]
```

### Not Supported: Anti-Quotation and Pattern Matching

**Anti-Quotation (Splicing)**: Not functional - produces parse errors
```haskell
-- This does NOT work for Java grammar:
let e1 = [expression| a |]
let e2 = [expression| $e1 + b |]  -- Parse error
```

**Pattern Matching**: Not functional - produces parse errors
```haskell
-- This does NOT work for Java grammar:
case expr of
    [expression| $e1 + $e2 |] -> ...  -- Parse error
```

Only construction mode is currently supported for the Java quasi-quoter.

## Extending Tests

To add more quasi-quotation tests:

1. **Check available rules**: Look at java.pg for rule names
2. **Use camelCase names**: Rule `StatementBlock` becomes quasi-quoter `statementBlock`
3. **Include complete syntax**: Statements need semicolons, etc.
4. **Test incrementally**: Start simple, add complexity gradually
5. **Respect reserved keywords**: Can't use `type` (Haskell keyword)

### Available Quasi-Quoters

Based on the Java grammar (partial list):

- `[expression| x + y |]` - Expressions
- `[statement| return x; |]` - Statements
- `[statementBlock| { ... } |]` - Code blocks
- `[compoundName| java.util.List |]` - Package/class names
- `[modifier| public |]` - Access modifiers
- `[literal| "hello" |]` - Literals

**Note**: Pattern matching is not currently supported for the Java grammar.

## References

- **Grammar file**: `test-grammars/java.pg`
- **QQ generator**: `GenQ.hs`
- **Example usage**: `test-grammars/p-main.hs` (for P language)
- **Previous removal**: commit 98c9397
- **Parser fixes**: commit f39a9cc

## Testing Strategy

1. **Start simple**: Test basic expressions and statements first
2. **Add complexity gradually**:
   - Single arguments → multiple arguments
   - Simple types → generic types
   - Single statements → blocks
3. **Test both construction and pattern matching**
4. **Verify with actual Java files**: Ensure quasi-quoted code matches what the parser accepts from files

## Known Limitations

- Quasi-quotations are compile-time only (Template Haskell)
- Error messages can be cryptic for syntax errors
- The quasi-quoter uses the same parser as file parsing, so any grammar ambiguities will affect both
