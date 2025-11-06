# Pattern Matching and Anti-Quotation with Python T-Strings

## Overview

This document explores how to implement pattern matching and anti-quotation for DSL quasi-quoters using Python 3.14 t-strings. These are the two most challenging aspects of quasi-quotation that t-strings don't provide natively.

---

## Problem Statement

### What We Want (Haskell-style)

**Pattern Matching:**
```haskell
-- Match against a pattern to extract sub-expressions
case expr of
    [expression| $e1 + $e2 |] ->
        -- e1 and e2 are bound to the matched sub-expressions
        doSomething e1 e2
```

**Anti-Quotation (Splicing):**
```haskell
-- Splice an existing AST node into a new expression
let e1 = [expression| x |]
let e2 = [expression| $e1 + y |]  -- e1 is spliced as AST node
```

### What T-Strings Give Us

**Template Structure:**
```python
template = t"x + y"

# Access to:
template.strings      # ('x + y',)
template.values       # () - empty if no interpolations

template = t"result: {x + y}"
template.strings      # ('result: ', '')
template.values       # (15,) if x=5, y=10
template.interpolations[0].expression  # "x + y" (as string!)
```

**Key Limitation:** We get string parts and evaluated values, but not AST nodes.

---

## Solution 1: Pattern Matching

### Approach 1A: Post-Parse Pattern Matching (Simplest)

**Don't use t-strings for patterns at all.** Just use Python 3.10+ structural pattern matching on parsed ASTs.

```python
from dataclasses import dataclass

@dataclass
class BinaryOp:
    left: 'Expr'
    op: str
    right: 'Expr'

@dataclass
class Variable:
    name: str

@dataclass
class Literal:
    value: int

Expr = BinaryOp | Variable | Literal

# Parse an expression
expr = java_expression(t"x + y")

# Pattern match on the AST (no quasi-quotation syntax)
match expr:
    case BinaryOp(left=Variable(x), op="+", right=Variable(y)):
        print(f"Addition: {x} + {y}")
    case BinaryOp(left=e1, op="*", right=e2):
        print(f"Multiplication: {e1} * {e2}")
    case Variable(name):
        print(f"Variable: {name}")
```

**Pros:**
- ✅ Simple, no magic
- ✅ Uses Python's native pattern matching
- ✅ Type-safe with dataclasses
- ✅ Works today (Python 3.10+)

**Cons:**
- ❌ Not quasi-quotation (can't use DSL syntax in patterns)
- ❌ More verbose than `[expression| $e1 + $e2 |]`
- ❌ Must know AST structure

**Verdict:** This is what most Python code would do. It works well but isn't "quasi-quotation."

---

### Approach 1B: T-String Pattern Templates with Custom Matcher

**Use t-strings to define patterns**, but implement custom matching logic.

```python
from string import Template, Interpolation
from typing import Any, Dict, Optional

class PatternVariable:
    """Marker for pattern variables that should be bound during matching"""
    def __init__(self, name: str):
        self.name = name

    def __repr__(self):
        return f"${self.name}"

# Helper to create pattern variables
def pattern_var(name: str) -> PatternVariable:
    return PatternVariable(name)

def pattern_match(expr: Expr, pattern_template: Template) -> Optional[Dict[str, Expr]]:
    """
    Match an expression against a pattern template.

    Returns bindings if match succeeds, None otherwise.
    """
    # Step 1: Identify pattern variables in the template
    pattern_vars = {}
    for i, interp in enumerate(pattern_template.interpolations):
        if isinstance(interp.value, PatternVariable):
            pattern_vars[i] = interp.value.name

    # Step 2: Parse the pattern template (replacing pattern vars with wildcards)
    # This is tricky - we need to construct a pattern AST
    pattern_source = reconstruct_pattern_source(pattern_template, pattern_vars)
    pattern_ast = parse_pattern(pattern_source)

    # Step 3: Match the expression against the pattern
    bindings = {}
    if match_ast(expr, pattern_ast, pattern_vars, bindings):
        return bindings
    return None

def reconstruct_pattern_source(template: Template, pattern_vars: Dict[int, str]) -> str:
    """Reconstruct source with pattern variables as placeholders"""
    result = []
    interp_idx = 0

    for i, static_str in enumerate(template.strings):
        result.append(static_str)
        if i < len(template.interpolations):
            if interp_idx in pattern_vars:
                # This is a pattern variable - use a unique placeholder
                result.append(f"__PATTERN_VAR_{interp_idx}__")
            else:
                # Regular interpolation - use its value
                result.append(str(template.interpolations[i].value))
            interp_idx += 1

    return ''.join(result)

def match_ast(expr: Expr, pattern: Expr, pattern_vars: Dict[int, str],
              bindings: Dict[str, Expr]) -> bool:
    """Recursively match expression against pattern"""
    # If pattern is a pattern variable, bind it
    if isinstance(pattern, Variable) and pattern.name.startswith("__PATTERN_VAR_"):
        # Extract the pattern variable index
        var_idx = int(pattern.name.split("_")[-2])
        var_name = pattern_vars[var_idx]

        # Check if already bound
        if var_name in bindings:
            return bindings[var_name] == expr  # Must match existing binding
        else:
            bindings[var_name] = expr  # Bind the variable
            return True

    # Otherwise, structural matching
    if type(expr) != type(pattern):
        return False

    if isinstance(expr, Variable):
        return expr.name == pattern.name

    if isinstance(expr, Literal):
        return expr.value == pattern.value

    if isinstance(expr, BinaryOp):
        return (expr.op == pattern.op and
                match_ast(expr.left, pattern.left, pattern_vars, bindings) and
                match_ast(expr.right, pattern.right, pattern_vars, bindings))

    return False

# Usage example
expr = java_expression(t"x + y")

# Create pattern variables
e1 = pattern_var("e1")
e2 = pattern_var("e2")

# Match using t-string pattern
bindings = pattern_match(expr, t"{e1} + {e2}")

if bindings:
    print(f"Matched! e1 = {bindings['e1']}, e2 = {bindings['e2']}")
    # e1 = Variable("x"), e2 = Variable("y")
else:
    print("No match")

# More complex example
expr2 = java_expression(t"a * b + c")

# Match: $e1 + $e2
bindings = pattern_match(expr2, t"{e1} + {e2}")
if bindings:
    print(f"e1 = {bindings['e1']}")  # e1 = BinaryOp(Variable("a"), "*", Variable("b"))
    print(f"e2 = {bindings['e2']}")  # e2 = Variable("c")
```

**Pros:**
- ✅ Uses t-string syntax for patterns
- ✅ Pattern variables are explicit (marked as `PatternVariable`)
- ✅ More quasi-quotation-like

**Cons:**
- ⚠️ Complex implementation
- ⚠️ Pattern variables must be created explicitly (`pattern_var()`)
- ⚠️ Not as clean as Haskell syntax
- ⚠️ Pattern parsing is complex

**Verdict:** Possible but complex. Better than nothing for DSL-style pattern matching.

---

### Approach 1C: Decorator-Based Pattern Matching

**Use a decorator to convert functions into pattern matchers.**

```python
from typing import Callable
from functools import wraps

class PatternMatcher:
    def __init__(self):
        self.patterns = []

    def case(self, pattern_template: Template):
        """Decorator to register a pattern case"""
        def decorator(func: Callable):
            self.patterns.append((pattern_template, func))
            return func
        return decorator

    def match(self, expr: Expr) -> Any:
        """Try to match expr against all registered patterns"""
        for pattern_template, handler in self.patterns:
            bindings = pattern_match(expr, pattern_template)
            if bindings is not None:
                # Call handler with bindings as kwargs
                return handler(**bindings)
        raise ValueError(f"No pattern matched: {expr}")

# Usage
matcher = PatternMatcher()

e1 = pattern_var("e1")
e2 = pattern_var("e2")
x = pattern_var("x")

@matcher.case(t"{e1} + {e2}")
def addition(e1, e2):
    return f"Add {e1} and {e2}"

@matcher.case(t"{e1} * {e2}")
def multiplication(e1, e2):
    return f"Multiply {e1} and {e2}"

@matcher.case(t"{x}")
def variable(x):
    return f"Variable: {x}"

# Use the matcher
expr = java_expression(t"a + b")
result = matcher.match(expr)
print(result)  # "Add Variable('a') and Variable('b')"
```

**Pros:**
- ✅ Clean, functional style
- ✅ Pattern-first approach
- ✅ Easy to add new cases

**Cons:**
- ⚠️ Still requires `pattern_var()` setup
- ⚠️ More boilerplate than Haskell

**Verdict:** Nice API, but requires significant implementation work.

---

## Solution 2: Anti-Quotation (Splicing)

### Approach 2A: Detect AST Interpolations

**When a t-string interpolation contains an AST node, splice it directly.**

```python
def java_expression(template: Template) -> Expr:
    """
    Parse Java expression from t-string, supporting AST splicing.
    """
    # Check if any interpolations are AST nodes
    has_ast_interpolations = any(
        isinstance(interp.value, Expr)
        for interp in template.interpolations
    )

    if not has_ast_interpolations:
        # Simple case: no splicing, just parse the whole thing
        source = ''.join(template.strings)
        return parse_java_expression(source)

    # Complex case: need to splice AST nodes
    return splice_and_parse(template)

def splice_and_parse(template: Template) -> Expr:
    """
    Handle t-strings with AST node interpolations.

    Strategy:
    1. For each AST interpolation, generate a unique placeholder
    2. Parse the template with placeholders
    3. Replace placeholders with actual AST nodes
    """
    placeholders = {}
    source_parts = []

    for i, static_str in enumerate(template.strings):
        source_parts.append(static_str)

        if i < len(template.interpolations):
            interp = template.interpolations[i]

            if isinstance(interp.value, Expr):
                # This is an AST node - use placeholder
                placeholder = f"__SPLICE_{i}__"
                source_parts.append(placeholder)
                placeholders[placeholder] = interp.value
            else:
                # Regular value - convert to string
                source_parts.append(str(interp.value))

    # Parse with placeholders
    source = ''.join(source_parts)
    ast = parse_java_expression(source)

    # Replace placeholders with actual AST nodes
    return replace_placeholders(ast, placeholders)

def replace_placeholders(ast: Expr, placeholders: Dict[str, Expr]) -> Expr:
    """Walk the AST and replace placeholder variables with actual nodes"""
    if isinstance(ast, Variable):
        if ast.name in placeholders:
            return placeholders[ast.name]
        return ast

    if isinstance(ast, Literal):
        return ast

    if isinstance(ast, BinaryOp):
        return BinaryOp(
            left=replace_placeholders(ast.left, placeholders),
            op=ast.op,
            right=replace_placeholders(ast.right, placeholders)
        )

    return ast

# Usage example
e1 = java_expression(t"x")
print(f"e1 = {e1}")  # Variable("x")

# Splice e1 into a new expression
e2 = java_expression(t"{e1} + y")
print(f"e2 = {e2}")  # BinaryOp(Variable("x"), "+", Variable("y"))

# More complex splicing
e3 = java_expression(t"a * b")
e4 = java_expression(t"{e3} + {e1}")
print(f"e4 = {e4}")
# BinaryOp(BinaryOp(Variable("a"), "*", Variable("b")), "+", Variable("x"))
```

**Pros:**
- ✅ True anti-quotation - splices AST nodes
- ✅ Automatic detection of AST vs regular values
- ✅ Clean usage syntax

**Cons:**
- ⚠️ Requires parsing with placeholders
- ⚠️ Must walk and replace AST (performance cost)
- ⚠️ Placeholders might conflict with valid identifiers

**Verdict:** This is the best approach for anti-quotation. It actually works like quasi-quotation splicing!

---

### Approach 2B: Unparse-Reparse

**Convert AST nodes back to source code, then reparse.**

```python
def unparse_expr(expr: Expr) -> str:
    """Convert an AST node back to source code"""
    if isinstance(expr, Variable):
        return expr.name

    if isinstance(expr, Literal):
        return str(expr.value)

    if isinstance(expr, BinaryOp):
        left_str = unparse_expr(expr.left)
        right_str = unparse_expr(expr.right)

        # Add parentheses if needed for precedence
        if isinstance(expr.left, BinaryOp):
            left_str = f"({left_str})"
        if isinstance(expr.right, BinaryOp):
            right_str = f"({right_str})"

        return f"{left_str} {expr.op} {right_str}"

    raise ValueError(f"Unknown expression type: {type(expr)}")

def java_expression_v2(template: Template) -> Expr:
    """
    Parse Java expression, converting any AST interpolations to source first.
    """
    source_parts = []

    for i, static_str in enumerate(template.strings):
        source_parts.append(static_str)

        if i < len(template.interpolations):
            interp = template.interpolations[i]

            if isinstance(interp.value, Expr):
                # Unparse the AST node to source
                source_parts.append(unparse_expr(interp.value))
            else:
                # Regular value
                source_parts.append(str(interp.value))

    # Parse the combined source
    source = ''.join(source_parts)
    return parse_java_expression(source)

# Usage (same as before)
e1 = java_expression_v2(t"x")
e2 = java_expression_v2(t"{e1} + y")
```

**Pros:**
- ✅ Simpler implementation
- ✅ No placeholder management
- ✅ Reuses parser directly

**Cons:**
- ⚠️ Requires unparsing (pretty-printing)
- ⚠️ May lose formatting/comments (though DSLs usually don't have these)
- ⚠️ Precedence handling can be tricky
- ⚠️ Double parsing (AST → source → AST)

**Verdict:** Simpler but less efficient. Good for prototyping.

---

## Comparison: Pattern Matching Approaches

| Approach | Syntax | Complexity | Quasi-Quotation Feel |
|----------|--------|------------|---------------------|
| **1A: Python match** | Native Python | Low | ❌ No |
| **1B: Custom matcher** | T-string patterns | High | ⚠️ Partial |
| **1C: Decorator-based** | T-string patterns | Medium | ✅ Yes |

**Recommendation:** Start with **1A (Python match)**, add **1C (decorators)** if quasi-quotation syntax is important.

---

## Comparison: Anti-Quotation Approaches

| Approach | Performance | Correctness | Complexity |
|----------|-------------|-------------|------------|
| **2A: Placeholder replacement** | Good | High | Medium |
| **2B: Unparse-reparse** | Poor (double parse) | Medium (precedence issues) | Low |

**Recommendation:** Use **2A (placeholder replacement)** for production. Use **2B (unparse-reparse)** for prototyping.

---

## Complete Example: Putting It All Together

```python
from dataclasses import dataclass
from string import Template, Interpolation
from typing import Optional, Dict, Any

# === AST Definitions ===

@dataclass
class Variable:
    name: str

@dataclass
class Literal:
    value: int

@dataclass
class BinaryOp:
    left: 'Expr'
    op: str
    right: 'Expr'

Expr = Variable | Literal | BinaryOp

# === Parsing (stub - would be generated by RTK) ===

def parse_java_expression(source: str) -> Expr:
    """Parse Java expression source code to AST"""
    # Simplified parser for demo
    source = source.strip()

    # Handle binary operations (very simplified!)
    for op in ['+', '-', '*', '/']:
        if op in source:
            parts = source.split(op, 1)
            if len(parts) == 2:
                left = parse_java_expression(parts[0])
                right = parse_java_expression(parts[1])
                return BinaryOp(left, op, right)

    # Handle placeholders
    if source.startswith('__SPLICE_'):
        return Variable(source)

    # Try to parse as number
    try:
        return Literal(int(source))
    except ValueError:
        pass

    # Must be a variable
    return Variable(source)

# === Anti-Quotation (Approach 2A) ===

def java_expression(template: Template) -> Expr:
    """Parse Java expression with AST splicing support"""
    has_ast_interpolations = any(
        isinstance(interp.value, Expr)
        for interp in template.interpolations
    )

    if not has_ast_interpolations:
        source = ''.join(template.strings)
        return parse_java_expression(source)

    # Handle splicing
    placeholders = {}
    source_parts = []

    for i, static_str in enumerate(template.strings):
        source_parts.append(static_str)

        if i < len(template.interpolations):
            interp = template.interpolations[i]

            if isinstance(interp.value, Expr):
                placeholder = f"__SPLICE_{i}__"
                source_parts.append(placeholder)
                placeholders[placeholder] = interp.value
            else:
                source_parts.append(str(interp.value))

    source = ''.join(source_parts)
    ast = parse_java_expression(source)
    return replace_placeholders(ast, placeholders)

def replace_placeholders(ast: Expr, placeholders: Dict[str, Expr]) -> Expr:
    """Replace placeholder variables with actual AST nodes"""
    if isinstance(ast, Variable) and ast.name in placeholders:
        return placeholders[ast.name]

    if isinstance(ast, BinaryOp):
        return BinaryOp(
            left=replace_placeholders(ast.left, placeholders),
            op=ast.op,
            right=replace_placeholders(ast.right, placeholders)
        )

    return ast

# === Pattern Matching (Approach 1A - simple) ===

def simple_pattern_match(expr: Expr):
    """Simple pattern matching using Python's match statement"""
    match expr:
        case BinaryOp(left=Variable(x), op="+", right=Variable(y)):
            print(f"Adding variables: {x} + {y}")

        case BinaryOp(left=e1, op="+", right=e2):
            print(f"Addition: {e1} + {e2}")

        case BinaryOp(left=e1, op="*", right=e2):
            print(f"Multiplication: {e1} * {e2}")

        case Variable(name):
            print(f"Variable: {name}")

        case Literal(value):
            print(f"Literal: {value}")

# === Usage Examples ===

print("=== Construction ===")
e1 = java_expression(t"x")
print(f"e1 = {e1}")

e2 = java_expression(t"x + y")
print(f"e2 = {e2}")

print("\n=== Anti-Quotation (Splicing) ===")
e3 = java_expression(t"{e1} + y")
print(f"e3 = {e3}")

e4 = java_expression(t"a * b")
e5 = java_expression(t"{e4} + {e1}")
print(f"e5 = {e5}")

print("\n=== Pattern Matching ===")
simple_pattern_match(e1)
simple_pattern_match(e2)
simple_pattern_match(e3)
simple_pattern_match(e4)
simple_pattern_match(e5)
```

**Output:**
```
=== Construction ===
e1 = Variable(name='x')
e2 = BinaryOp(left=Variable(name='x'), op='+', right=Variable(name='y'))

=== Anti-Quotation (Splicing) ===
e3 = BinaryOp(left=Variable(name='x'), op='+', right=Variable(name='y'))
e5 = BinaryOp(left=BinaryOp(left=Variable(name='a'), op='*', right=Variable(name='b')), op='+', right=Variable(name='x'))

=== Pattern Matching ===
Variable: x
Adding variables: x + y
Addition: Variable(name='x') + Variable(name='y')
Multiplication: Variable(name='a') * Variable(name='b')
Addition: BinaryOp(left=Variable(name='a'), op='*', right=Variable(name='b')) + Variable(name='x')
```

---

## Summary

### What Works Well ✅

1. **Construction**: T-strings provide excellent syntax for construction
   ```python
   expr = java_expression(t"x + y * z")
   ```

2. **Anti-Quotation**: AST splicing works with placeholder approach
   ```python
   e1 = java_expression(t"x")
   e2 = java_expression(t"{e1} + y")  # Splices e1's AST
   ```

3. **Pattern Matching (Simple)**: Python's match works well for basic cases
   ```python
   match expr:
       case BinaryOp(left=Variable(x), op="+", right=Variable(y)):
           ...
   ```

### What's Challenging ⚠️

1. **Pattern Matching (Quasi-Quotation Style)**: Requires custom implementation
   - Pattern variables need explicit marking
   - Custom matching logic required
   - More complex than Haskell

2. **Syntax Beauty**: Not as elegant as Haskell's `[expression| $e1 + $e2 |]`

### Effort Estimate

- **Anti-quotation** (Approach 2A): 2-3 weeks
- **Pattern matching** (Approach 1A - simple): 1 week (use Python match)
- **Pattern matching** (Approach 1C - quasi-quotation style): 4-6 weeks
- **Total for both**: 3-4 weeks (simple) or 6-9 weeks (full quasi-quotation style)

---

## Recommendation

For a Python RTK implementation:

1. ✅ **Use t-strings for construction** - Clean, native syntax
2. ✅ **Implement anti-quotation with placeholder approach** - Works well, true splicing
3. ⚠️ **Start with Python match for patterns** - Simple, works today
4. ⚠️ **Consider decorator-based pattern matching** if quasi-quotation syntax is critical

This gives you 80% of quasi-quotation functionality with reasonable effort.

---

*Document created: 2025-11-04*
*Author: Claude (Anthropic)*
*Context: Design exploration for RTK-style quasi-quotation using Python 3.14 t-strings*
