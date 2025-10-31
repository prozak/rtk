# Assignment Operator Fix

## Issue

When testing the method body: `public void setValue(int value) { this.value = value; }`, parsing would fail due to THREE issues:
1. The grammar was missing the simple assignment operator `=`
2. The grammar had a reduce/reduce conflict in how assignments were parsed
3. The grammar had indirect left recursion in Primary/FieldAccess that prevented `this.value` from parsing

## Root Cause

The `AssignmentOp` rule in `test-grammars/java.pg` (line 260-268) only included compound assignment operators:
- `+=`, `-=`, `*=`, `|=`, `&=`, `^=`, `%=`

But was **missing**:
- `=` - Simple assignment (most commonly used!)
- `/=` - Division assignment
- `<<=`, `>>=`, `>>>=` - Shift assignments

This meant that any method body using simple assignment (like `this.value = value;`) would fail to parse.

## Test Case

**File:** `test-grammars/java/test-set-value.java`
```java
public class TestSetValue {
    private int value;
    public void setValue(int value) { this.value = value; }
}
```

This test uses:
1. Field access on left-hand side: `this.value`
2. Simple assignment operator: `=`
3. Parameter reference on right-hand side: `value`

## Solution

### Part 1: Add Missing Assignment Operators

Updated `AssignmentOp` in `test-grammars/java.pg` to include all Java assignment operators:

```
AssignmentOp =
 '='          # Simple assignment (ADDED)
 | '+='       # Add and assign
 | '-='       # Subtract and assign
 | '*='       # Multiply and assign
 | '/='       # Divide and assign (ADDED)
 | '|='       # Bitwise OR and assign
 | '&='       # Bitwise AND and assign
 | '^='       # Bitwise XOR and assign
 | '%='       # Modulo and assign
 | '<<='      # Left shift and assign (ADDED)
 | '>>='      # Right shift and assign (ADDED)
 | '>>>=' ;   # Unsigned right shift and assign (ADDED)
```

### Part 2: Eliminate Reduce/Reduce Conflict

The original grammar structure created ambiguity:

**Before (BROKEN):**
```
AssignmentExpression =
  ConditionalExpression
  | Assignment ;

Assignment = LeftHandSide AssignmentOp Expression;

LeftHandSide = CompoundName | FieldAccess | ArrayAccess ;
```

**Problem:** When the parser saw `this.value`, it had two conflicting derivations:
1. `ConditionalExpression` → ... → `Primary` → `FieldAccess`
2. `Assignment` → `LeftHandSide` → `FieldAccess`

The parser couldn't decide whether to reduce `FieldAccess` as a complete expression or keep it as a `LeftHandSide` for potential assignment. This is a **reduce/reduce conflict**.

**After (FIXED - Attempt 1 - Still had ambiguity):**
```
AssignmentExpression =
  ConditionalExpression
  | ConditionalExpression AssignmentOp AssignmentExpression ;
```

This still created ambiguity because both alternatives start with `ConditionalExpression`.

**After (FINAL FIX):**
```
AssignmentExpression =
  ConditionalExpression (AssignmentOp AssignmentExpression)? ;
```

**Solution:** Eliminated the ambiguity by:
1. Removing the separate `LeftHandSide` and `Assignment` rules
2. Using an optional pattern `(AssignmentOp AssignmentExpression)?` instead of alternatives
3. This creates a single, unambiguous parse path: parse `ConditionalExpression`, then optionally match assignment operator and right-hand side

Now the parser has only one production rule for AssignmentExpression, eliminating all ambiguity. This follows the standard approach used in many parser generators where optional elements eliminate the need for multiple alternatives.

### Part 3: Eliminate Indirect Left Recursion

Even after the above fixes, parsing still failed! The grammar had **indirect left recursion**:

**Before (BROKEN):**
```
Primary =
  ... | FieldAccess | MethodInvocation | ArrayAccess | ...

FieldAccess = Primary '.' id | 'super' '.' id ;
MethodInvocation = ... | Primary '.' id '(' Arglist ')' | ... ;
ArrayAccess = ... | Primary '[' Expression ']' ;
```

**Problem:** The cycle `Primary → FieldAccess → Primary '.' id` creates indirect left recursion. This is why `test-return-field.java` worked (it used `return value;` which is just CompoundName) but `this.value = value;` failed (it requires FieldAccess).

**After (FIXED):**
```
PostfixExpression =
  PrimaryNoPostfix
  | PostfixExpression PostfixOp
  | PostfixExpression '.' id              # Field access
  | PostfixExpression '.' id '(' Arglist ')'  # Method call
  | PostfixExpression '[' Expression ']' ;    # Array access

PrimaryNoPostfix =
  Literal | 'this' | '(' Expression ')' | CreationExpression
  | CompoundName '(' Arglist ')' | CompoundName
  | 'super' '.' id | 'super' '.' id '(' Arglist ')' ;
```

**Solution:**
1. Created `PrimaryNoPostfix` with only base expressions (no recursive references)
2. Moved field access, method calls, and array access into `PostfixExpression` as postfix operations
3. This eliminates indirect recursion while keeping left recursion in PostfixExpression itself (which LALR parsers handle correctly)

Now `this.value` parses as:
- `this` → PrimaryNoPostfix → PostfixExpression
- PostfixExpression '.' id → `this.value`

### Part 4: Fix CompoundName Ambiguity

After fixing the indirect recursion, there was still an ambiguity in `PrimaryNoPostfix`:

**Before:**
```
PrimaryNoPostfix =
  ... | CompoundName '(' Arglist ')' | CompoundName | ...
```

When the parser saw `CompoundName`, it couldn't decide whether to:
1. Reduce as `PrimaryNoPostfix → CompoundName`
2. Wait for `'(' Arglist ')'` to match `PrimaryNoPostfix → CompoundName '(' Arglist ')'`

**After:**
```
PrimaryNoPostfix =
  ... | CompoundName ('(' Arglist ')')? | ...
```

Using the optional pattern eliminates the ambiguity - parse `CompoundName`, then optionally match the argument list.

## Impact

This fix enables parsing of:
- Simple assignment: `x = 5;`
- Field assignment: `this.field = value;`
- Array assignment: `arr[i] = x;`
- All compound assignments: `/=`, `<<=`, `>>=`, `>>>=`

Combined with the recent method body parsing fix (PR #45), this completes support for basic method implementations with assignments.

## Historical Context

From commit `3e537a0` (Add method body test cases for incremental parsing coverage):
> Note: Assignment statements are not included as the grammar currently only supports compound assignment operators (+=, -=) but not simple assignment (=) for statement-level assignments.

This issue was documented but not fixed. This commit resolves that limitation.

## Related Files

- Grammar: `test-grammars/java.pg` (line 260-272)
- Test case: `test-grammars/java/test-set-value.java`
- Makefile: Updated with `test-java-set-value` target
- Documentation: `docs/method-body-parsing-issue.md` (background)

## Testing

To test this fix, run:
```bash
make test-java-set-value
```

The test should now successfully parse the `setValue` method with simple assignment.
