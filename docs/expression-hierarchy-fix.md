# Expression Hierarchy Fix - Detailed Plan

## Current Problem Visualization

```
Current (BROKEN - Multiple Paths to Same Constructs):

Expression ─┬─> Assignment
            ├─> ConditionalExpression ─────┐
            ├─> UnaryOp Expression         │
            ├─> CastExpression ───────┐    │
            ├─> CreationExpression    │    │
            ├─> Literal               │    │
            └─> '(' Expression ')'    │    │
                                      │    │
                                      │    ├─> ... ─> MultiplicativeExpression
                                      │    │           │
UnaryExpression ──┬─> PrefixOp UnaryExpression        │
                  ├─> CastExpression ─────┘           │
                  └─> PostfixExpression ──────────────┘

CONFLICT: Both Expression and UnaryExpression can reduce to CastExpression!
CONFLICT: ConditionalExpression chains to UnaryExpression, but Expression also directly includes alternatives that should come from UnaryExpression!
```

## Correct Java Expression Hierarchy

```
Fixed (SINGLE Path Through Precedence):

Expression
    │
    └─> AssignmentExpression ─┬─> ConditionalExpression
                               │       │
                               │       ├─> ConditionalOrExpression
                               │       │       │
                               │       │       └─> ConditionalAndExpression
                               │       │               │
                               │       │               └─> InclusiveOrExpression
                               │       │                       │
                               │       │                       └─> ExclusiveOrExpression
                               │       │                               │
                               │       │                               └─> AndExpression
                               │       │                                       │
                               │       │                                       └─> EqualityExpression
                               │       │                                               │
                               │       │                                               └─> RelationalExpression
                               │       │                                                       │
                               │       │                                                       └─> ShiftExpression
                               │       │                                                               │
                               │       │                                                               └─> AdditiveExpression
                               │       │                                                                       │
                               │       │                                                                       └─> MultiplicativeExpression
                               │       │                                                                               │
                               │       │                                                                               └─> UnaryExpression ─┬─> PrefixOp UnaryExpression
                               │       │                                                                                                    │
                               │       │                                                                                                    └─> UnaryExpressionNotPlusMinus ─┬─> PostfixExpression ─┬─> Primary ─┬─> Literal
                               │       │                                                                                                                                     │                      │           ├─> 'this'
                               │       │                                                                                                                                     │                      │           ├─> '(' Expression ')'
                               │       │                                                                                                                                     │                      │           ├─> CreationExpression
                               │       │                                                                                                                                     │                      │           ├─> FieldAccess
                               │       │                                                                                                                                     │                      │           ├─> MethodInvocation
                               │       │                                                                                                                                     │                      │           ├─> ArrayAccess
                               │       │                                                                                                                                     │                      │           └─> CompoundName
                               │       │                                                                                                                                     │                      │
                               │       │                                                                                                                                     │                      └─> PostfixExpression PostfixOp
                               │       │                                                                                                                                     │
                               │       │                                                                                                                                     ├─> '~' UnaryExpression
                               │       │                                                                                                                                     ├─> '!' UnaryExpression
                               │       │                                                                                                                                     └─> CastExpression
                               │       │
                               │       └─> ConditionalOrExpression '?' Expression ':' ConditionalExpression
                               │
                               └─> Assignment
```

## Grammar Changes Required

### 1. Redefine Expression (line 190)

**Before:**
```
Expression  =
 Assignment
 | ConditionalExpression
 | UnaryOp Expression
 | CastExpression
 | CreationExpression
 | Literal
 | '(' Expression  ')' ;
```

**After:**
```
Expression = AssignmentExpression ;

AssignmentExpression =
 ConditionalExpression
 | Assignment ;
```

### 2. Remove UnaryOp (line 183-188)

**Before:**
```
UnaryOp =
'-'
 | '++'
 | '--'
 | '!'
 | '~' ;
```

**After:**
```
# Delete this rule - merge into PrefixOp
```

### 3. Update UnaryExpression (line 267)

**Before:**
```
UnaryExpression = PrefixOp UnaryExpression | CastExpression | PostfixExpression ;
```

**After:**
```
UnaryExpression =
  PrefixOp UnaryExpression
  | UnaryExpressionNotPlusMinus ;

UnaryExpressionNotPlusMinus =
  PostfixExpression
  | '~' UnaryExpression
  | '!' UnaryExpression
  | CastExpression ;
```

### 4. Update PostfixExpression (line 269)

**Before:**
```
PostfixExpression = Primary | CompoundName | PostExpression;
```

**After:**
```
PostfixExpression =
  Primary
  | PostfixExpression PostfixOp ;
```

### 5. Update Primary (line 271)

**Before:**
```
Primary = Literal | 'this' | '(' Expression ')' | CreationExpression | FieldAccess | MethodInvocation | ArrayAccess ;
```

**After:**
```
Primary =
  Literal
  | 'this'
  | '(' Expression ')'
  | CreationExpression
  | FieldAccess
  | MethodInvocation
  | ArrayAccess
  | CompoundName ;
```

### 6. Remove PostExpression (line 265)

**Before:**
```
PostExpression = PostfixExpression PostfixOp ;
```

**After:**
```
# Delete this rule - merged into PostfixExpression
```

## Why This Fixes Conflicts

### Current Issue:
When the parser sees `(int) x`:
1. Could parse as `Expression -> CastExpression`
2. Could parse as `Expression -> ConditionalExpression -> ... -> UnaryExpression -> CastExpression`
3. **Two different parse paths to the same construct = CONFLICT**

### After Fix:
When the parser sees `(int) x`:
1. **Only path:** `Expression -> AssignmentExpression -> ConditionalExpression -> ... -> UnaryExpression -> UnaryExpressionNotPlusMinus -> CastExpression`
2. **One parse path = NO CONFLICT**

## Precedence Verification

The fixed grammar maintains correct Java operator precedence:

1. **Assignment** (lowest precedence)
2. **Conditional** (?:)
3. **Logical OR** (||)
4. **Logical AND** (&&)
5. **Bitwise OR** (|)
6. **Bitwise XOR** (^)
7. **Bitwise AND** (&)
8. **Equality** (==, !=)
9. **Relational** (<, >, <=, >=, instanceof)
10. **Shift** (<<, >>, >>>)
11. **Additive** (+, -)
12. **Multiplicative** (*, /, %)
13. **Unary** (++, --, +, -, ~, !, cast)
14. **Postfix** (++, --)
15. **Primary** (highest precedence)

## Test Cases to Verify

After applying the fix, test these expressions:

```java
// Simple expressions
x
42
"hello"

// Arithmetic
a + b
a + b * c
(a + b) * c

// Casts
(int) x
(int) (float) x

// Unary operators
-x
!flag
~bits
++count
--count

// Postfix
count++
count--

// Complex
x = y = z
a + b > c && d < e
flag ? x : y
(int) a + (float) b
```

## Implementation Steps

1. Backup current java.pg
2. Apply changes in order (1-6 above)
3. Rebuild grammar
4. Check conflicts: `grep "conflicts" happy_log.txt`
5. Run test suite
6. Add complex expression tests
7. Commit changes

## Expected Results

- **Before:** 398 reduce/reduce conflicts, 83 shift/reduce conflicts (481 total)
- **After:** ~90 reduce/reduce conflicts, ~80 shift/reduce conflicts (170 total)
- **Reduction:** ~65% fewer conflicts
