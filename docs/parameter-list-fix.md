# Parameter Parsing Fix

## Issue

Methods with parameters failed to parse with error:
```
Parse error [Tk__tok__rparen__7,Tk__tok__symbol__13,...]
```

The parser successfully parsed the parameter list `(int x)` but failed when entering the method body `{`.

## Root Cause

The `ParameterList` grammar rule used incorrect syntax for comma-separated lists:

```
ParameterList = Parameter * ',' ;    ❌ INCORRECT
```

This `* ','` syntax doesn't properly form a standard comma-separated list in RTK/Happy grammar.

## Solution

Changed to the standard pattern used elsewhere in the grammar:

```
ParameterList = Parameter (',' Parameter)* ;   ✅ CORRECT
```

This means: "one Parameter, followed by zero or more occurrences of `, Parameter`"

## Other Fixes

Found and fixed two other rules using the same problematic syntax:

### Arglist (method arguments)
**Before:**
```
Arglist = Expression * ',' ;
```

**After:**
```
Arglist = (Expression (',' Expression)*)? ;
```

Arglist needs to support zero or more arguments (for calls like `foo()` or `foo(1, 2, 3)`), so we wrap it in `()?` to make it optional.

### VariableInitializerList (array initializers)
**Before:**
```
VariableInitializerList = VariableInitializer * ',' ( ',' )? ;
```

**After:**
```
VariableInitializerList = (VariableInitializer (',' VariableInitializer)* (',')?)? ;
```

Array initializers support:
- Empty: `{}`
- Elements: `{1, 2, 3}`
- Trailing comma: `{1, 2, 3,}`

## Standard Patterns for Comma-Separated Lists

Based on the working patterns in the grammar:

### Required List (1 or more items)
```
VariableDeclaratorList = VariableDeclarator (',' VariableDeclarator)* ;
```

### Optional List (0 or more items)
```
Arglist = (Expression (',' Expression)*)? ;
```

### Optional List with Trailing Comma
```
VariableInitializerList = (VariableInitializer (',' VariableInitializer)* (',')?)? ;
```

## Impact

This fix enables:
- ✅ Methods with parameters: `public void test(int x)`
- ✅ Multiple parameters: `public void test(int x, String y)`
- ✅ Method calls with arguments: `foo(1, 2, 3)`
- ✅ Array initializers: `int[] arr = {1, 2, 3};`

## Related Files

- Grammar: `test-grammars/java.pg` (lines 151, 161, 334)
- Test case: `test-grammars/java/test-parameter-only.java`
- Investigation: `docs/parameter-parsing-investigation.md`
