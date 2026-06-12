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

### Pattern Matching and Splicing

Pattern matching and anti-quotation (splicing), which used to fail with
parse errors for hierarchical grammars, work since the shared-type +
attach-point machinery landed (see `Normalize.computeQQAttachPoints` and
the summary block at the end of `java-qq-test.hs`). The metavariable
must name the type explicitly: `$Expression:x`.

- ✅ Pattern matching: `case expr of [expression| $Expression:l + $Expression:r |] -> ...`
- ✅ Splicing: `let e = [expression| x |] in [expression| $Expression:e + 1 |]`

Parts 2, 3 and 5 of `java-qq-test.hs` exercise both, and the rewrite
recipe below builds on them.

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
make test-java-rewrite   # the rewrite recipe (QQ patterns + SYB) below
```

This will:
1. Copy `java-qq-test.hs` (resp. `java-rewrite-test.hs`) to test-out
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

### Construction
Build AST nodes directly from Java syntax:
```haskell
let expr = [expression| x + y * z |]
let stmt = [statement| return x; |]
let block = [statementBlock| { x = 1; return x; } |]
```

### Anti-Quotation (Splicing)
A `$Type:name` metavariable in an expression quote splices a Haskell
variable of that AST type into the quoted syntax:
```haskell
let e1 = [expression| a |]
let e2 = [expression| $Expression:e1 + b |]
```

### Pattern Matching
The same metavariables in a pattern quote bind sub-trees; every source
position in the pattern is a wildcard, so patterns written in a quote
match ASTs parsed from anywhere:
```haskell
case expr of
    [expression| $Expression:l + $Expression:r |] -> ...  -- binds l and r
```

## Rewriting parsed Java (QQ patterns + SYB)

The pieces above combine into the rewrite recipe — the reason the toolkit
is called a *rewrite* toolkit. A rewrite is an ordinary function whose
match arms are quasi-quoted patterns and whose results are quasi-quoted
expressions; SYB applies it to every node of any AST value. No rtk-specific
API is involved: the generated parser derives `Data` for every AST type
(your project already depends on `syb` for it), so `Data.Generics` is the
traversal library, and the quasi-quoter contributes only the arms.

The worked example, exercised by `make test-java-rewrite`
(`test-grammars/java-rewrite-test.hs`): turn comparisons against `null`
into Yoda style, everywhere in a block.

```haskell
import Data.Generics (everywhere, everything, mkT, mkQ)

-- one arm per shape, Java on both sides; everything else passes through
yoda :: JP.Expression -> JP.Expression
yoda [J.expression| $Expression:x == null |] = [J.expression| null == $Expression:x |]
yoda [J.expression| $Expression:x != null |] = [J.expression| null != $Expression:x |]
yoda e = e

-- bottom-up over ANY value containing expressions: a block, a method,
-- a whole compilation unit
rewritten = everywhere (mkT yoda) body
```

Because positions are equality-transparent, the expected result can be
written as a quote too, and the test is two quotes and a traversal:

```haskell
body      = [J.statementBlock| { if (name == null) { return defaultName; } ... } |]
expected  = [J.statementBlock| { if (null == name) { return defaultName; } ... } |]
-- everywhere (mkT yoda) body == expected
```

The same patterns drive queries with `everything`/`mkQ` — e.g. counting
the comparisons the pass would rewrite (deliberately unused metavariables
are spelled with a leading underscore, `$Expression:_x`, to stay clean
under `-Wunused-matches`):

```haskell
pendingNullChecks [J.expression| $Expression:_x == null |] = 1
pendingNullChecks [J.expression| $Expression:_x != null |] = 1
pendingNullChecks _ = 0

everything (+) (0 `mkQ` pendingNullChecks) body  -- 2 before, 0 after
```

To rewrite several AST types in one pass, chain per-type functions with
`extT` (`everywhere (mkT onExpr `extT` onStmt)`) — rtk's own pipeline does
exactly this over its grammar AST (`cleanGrammarTokens` in
`src/generated/Frontend.hs`), and since task 8d the pipeline's own
clause-shape matches are QQ patterns (`Frontend.altElems`,
`StringLiterals.normalizeClause`, `Normalize.checkSoleElement`). For a
larger, end-to-end example, the write-you-a-haskell tutorial's evaluator
(`tutorials/write-you-a-haskell/lc-main.hs`) implements capture-avoiding
substitution and free-variable analysis this way: QQ arms for the binder
cases, `gmapT`/`gmapQ` generic recursion for everything else.

### Where patterns match (the chain-position rule)

In a hierarchical grammar the splice alternative sits at one rule of each
shared-type group — for `Expression` that is `PrimaryNoPostfix`, the
bottom of the precedence chain (see `Normalize.computeQQAttachPoints`).
A pattern metavariable therefore matches scrutinees whose corresponding
operand parses at that chain position: identifiers, literals, method
calls, parenthesized expressions. An operand produced higher in the chain
(say the `a + b` in `a + b == null`) does not reach the attach point, so
the pattern passes it by — matches simply fail, nothing breaks. When a
rewrite must catch every operand shape, parenthesize at the call site or
match on the printed form; `java-rewrite-test.hs` asserts both sides of
this boundary.

### Scalar vs. list metavariables

`$Type:x` binds a single node when the type's splice is scalar and a
whole list when the splice was registered by a list rule (`Type*`) — in
pattern context a list metavariable binds the entire remaining list, in
expression context it prepends a list. A type whose splice is list-shaped
cannot bind a lone (scalar) occurrence: the metavariable then matches
nothing and GHC reports the variable as unbound. Check the generated
`JavaQQ.hs`: `anti<Type>Pat` taking `[Type]` means list-shaped,
`Type` means scalar.

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

All of them work in expression and pattern context alike (see "Rewriting
parsed Java" above for patterns in anger).

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
