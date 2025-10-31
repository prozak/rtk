# Method Body Parsing Issue

## Summary

The Java grammar currently **cannot parse method bodies**, even though they are defined in the grammar. This affects all method declarations with implementations (non-abstract methods).

## Test Results

### Failing Test
**File:** `test-grammars/java/test-empty-method.java`
```java
public class TestEmptyMethod {
    public void test() {}
}
```

**Error:**
```
Parse error [Tk__tok__lparen__6,Tk__tok__rparen__7,Tk__tok__symbol__13,Tk__tok__symbol__14,Tk__tok__symbol__14]
```

The parser fails when it encounters the `(` token of the method declaration.

### Passing Tests
All current Java tests pass because they **only contain field declarations**, no methods:
- `Test.java` - fields only
- `TestBasic.java` - fields only
- `TestMinimal.java` - fields only
- `Simple.java` - fields only
- `Complex.java` - fields and inner class, no methods
- All `test-grammars/java/test-*.java` files - fields only

## Root Cause Analysis

### The Ambiguity Problem

In `FieldDeclaration` (lines 75-81), both `MethodDeclaration` and `VariableDeclaration` are alternatives:

```
FieldDeclaration  =
 (ModifierList ((  DocComment?  ( MethodDeclaration
 | ConstructorDeclaration
 | VariableDeclaration
 | ClassDeclaration ))
 | StaticInitializer))
 |  ';'  ;
```

**Method Declaration** (lines 85-88):
```
MethodDeclaration  =
 TypeParameters Type id
 '('  ParameterList  ')'  SquareBracketsList
 ( StatementBlock  |  ';'  )  ;
```

**Variable Declaration** (lines 98-100):
```
VariableDeclaration  =
 Type VariableDeclaratorList
   ';'  ;
```

### The Conflict

When `TypeParameters` is empty (which it often is for methods without generics), both rules begin identically:

- **Method:** `Type id (` ...
- **Variable:** `Type id ;` ...

When the parser sees:
```java
public void test
```

It cannot determine if this is:
1. A method declaration `public void test()` (look for `(`)
2. A variable declaration `public void test;` (look for `;`)

The parser must decide whether to:
- **Shift:** Continue reading to see if `(` follows (method)
- **Reduce:** Treat what we have as a variable declaration

### Why This Wasn't Caught

The conflict reduction work (documented in `conflict-reduction-results.md`) reduced conflicts from 481 to 149, but focused primarily on:
1. Expression hierarchy ambiguity (305+ conflicts eliminated)
2. Generics angle brackets vs comparison operators
3. Modifier list lookahead
4. If-else dangling-else problem

The MethodDeclaration vs VariableDeclaration ambiguity was **not addressed** in that work, possibly because:
- Tests only used field declarations (no methods)
- The conflict may have been masked by other conflicts
- The focus was on expression parsing, not member declarations

## Grammar Structure Issues

### Current Structure
```
FieldDeclaration → ModifierList → (MethodDeclaration | VariableDeclaration | ...)
MethodDeclaration → TypeParameters Type id '(' ...
VariableDeclaration → Type VariableDeclaratorList ...
```

**Problem:** Common prefix `Type id` creates ambiguity.

### Recommended Solution

Factor out the common prefix to eliminate ambiguity:

```
FieldDeclaration  =
 ModifierList MemberDeclaration
 |  ';'  ;

MemberDeclaration  =
 DocComment? MemberDecl ;

MemberDecl =
 TypeParameters TypeAndId MemberDeclRest
 | ConstructorDeclaration
 | ClassDeclaration
 | StaticInitializer ;

TypeAndId = Type id ;

MemberDeclRest =
 '(' ParameterList ')' SquareBracketsList ( StatementBlock | ';' )   # Method
 | SquareBracketsList OptVariableInitializer (',' VariableDeclarator)* ';'  # Variable ;
```

**Benefits:**
- Eliminates the ambiguity by factoring common prefix
- Parser only needs to look at the token after `Type id` to decide
- Maintains same AST structure
- More efficient parsing

### Alternative Solution

Keep current structure but ensure parser has sufficient lookahead:
- Requires 3-token lookahead: `Type id (`
- May require Happy parser flags: `%expect` to accept shift/reduce conflicts
- Less elegant but requires minimal grammar changes

## Statement and Expression Support

**Good news:** The grammar DOES define comprehensive statement and expression support:

### Statements (lines 128-187)
- ✅ Return statements
- ✅ Expression statements
- ✅ Statement blocks
- ✅ If/else statements
- ✅ While/do-while loops
- ✅ For loops
- ✅ Try-catch-finally
- ✅ Switch statements
- ✅ Break/continue
- ✅ Synchronized blocks
- ✅ Throw statements

### Expressions (lines 189-297)
- ✅ Assignment operators (=, +=, -=, etc.)
- ✅ Conditional expressions (? :)
- ✅ Logical operators (||, &&, |, &, ^)
- ✅ Comparison operators (==, !=, <, >, <=, >=)
- ✅ Arithmetic operators (+, -, *, /, %)
- ✅ Shift operators (<<, >>, >>>)
- ✅ Unary operators (++, --, +, -, ~, !)
- ✅ Type casts
- ✅ Method invocations
- ✅ Array access
- ✅ Field access
- ✅ Object creation (new)
- ✅ Literals (int, float, char, string, boolean, null)

**The grammar rules for statements and expressions are well-defined.** The issue is purely in the member declaration ambiguity, not in the statement/expression parsing.

## Impact

### What Works
- ✅ Class declarations
- ✅ Field declarations
- ✅ Import statements
- ✅ Package declarations
- ✅ Annotations
- ✅ Enums
- ✅ Generics (type parameters)
- ✅ Inner classes
- ✅ JavaDoc comments
- ✅ Abstract method declarations (with `;` instead of body)

### What Doesn't Work
- ❌ Method bodies with any implementation
- ❌ Constructor bodies
- ❌ Static initializer blocks (uses `StatementBlock`)
- ❌ Empty method bodies `{}`

### Workaround

For now, all methods must be declared abstract (ending with `;` instead of a body):

```java
// ❌ Doesn't parse
public void test() {}

// ✅ Parses (abstract method)
public void test();
```

## Recommended Actions

### Priority 1: Fix Grammar Ambiguity (CRITICAL)
**Effort:** Medium | **Impact:** Enables all method body parsing

1. Implement the "Factor Common Prefix" solution above
2. Refactor `FieldDeclaration` to eliminate Method/Variable ambiguity
3. Test with incrementally complex method bodies:
   - Empty body: `public void test() {}`
   - Simple return: `public int get() { return 42; }`
   - Variable declaration: `public void test() { int x = 5; }`
   - Method call: `public void test() { foo(); }`

### Priority 2: Add Comprehensive Method Tests
**Effort:** Low | **Impact:** Verification

Create test files for:
- `test-empty-method.java` - Already created, currently fails
- `test-simple-return.java` - Already exists, add to test suite
- `test-method-call.java` - Method calling another method
- `test-constructor.java` - Constructor with body
- `test-static-init.java` - Static initializer block

### Priority 3: Update CI
**Effort:** Low | **Impact:** Continuous verification

Once method parsing works:
- Add method body tests to `test-all-java` target
- Update CI to catch regressions
- Document grammar capabilities

## References

- **Grammar file:** `test-grammars/java.pg`
- **Test file (failing):** `test-grammars/java/test-empty-method.java`
- **Makefile target:** `test-java-empty-method`
- **Conflict analysis:** `docs/java-grammar-conflict-analysis.md`
- **Conflict reduction:** `docs/conflict-reduction-results.md`

## Historical Context

From commit `98c9397` (Work around Java parser issues in test files):

> Multiple issues were discovered with the Java parser during CI setup:
> 1. Quasi-quotations fail on multi-argument method calls
> 2. Parser fails on "return" keyword
> 3. Parser fails on method bodies with assignments

The issues were "worked around" by removing all method bodies from test files, not by fixing the grammar. This document identifies the root cause and provides a path forward to properly support method bodies.
