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

The java-qq-test.hs file includes the following quasi-quotation tests:

### Test 1: Simple Expression
```haskell
let expr1 = [expression| x + y |]
```
Tests basic binary operator expressions.

### Test 2: Method Call (Single Argument)
```haskell
let expr2 = [expression| obj.method(arg) |]
```
Tests method invocation with a single argument. This previously failed on multi-argument calls.

### Test 3: Literal Expression
```haskell
let expr3 = [expression| 42 |]
```
Tests simple integer literal expressions.

### Test 4: Return Statement
```haskell
let stmt1 = [statement| return; |]
```
Tests return statement parsing. This previously failed.

### Test 5: Assignment Statement
```haskell
let stmt2 = [statement| x = 5; |]
```
Tests simple assignment statements. This previously failed in method bodies.

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

## Extending Tests

To add more quasi-quotation tests:

1. **Check available rules**: Look at java.pg for rule names
2. **Use lowercase names**: Rule `Expression` becomes quasi-quoter `expression`
3. **Include complete syntax**: Statements need semicolons, etc.
4. **Test incrementally**: Start simple, add complexity gradually

### Suggested Additional Tests

Based on the grammar, these quasi-quoters should be available:

- `[type| int |]` - Type specifications
- `[compoundname| java.util.List |]` - Package/class names
- `[modifier| public |]` - Access modifiers
- `[literal| "hello" |]` - Literals
- `[block| { return x; } |]` - Code blocks

## Pattern Matching with Quasi-Quotations

Quasi-quotations can also be used for pattern matching:

```haskell
case expr of
    [expression| $id1 + $id2 |] ->
        putStrLn $ "Binary addition: " ++ show id1 ++ " + " ++ show id2
    _ -> putStrLn "Other expression"
```

Variables in patterns use `$varName` syntax to capture AST nodes.

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
