# Pattern Matching with Quasi-Quotations

## How Pattern Matching Works

Pattern matching with quasi-quotations uses a **prefix-matching system** to determine variable types.

### Variable Naming Convention

Pattern variables must be named with a **prefix that matches a grammar rule name** (case-insensitive).

The system works as follows:
1. When you write `$variable` in a pattern, it extracts "variable" as the name
2. It searches for the longest prefix of "variable" that matches a known rule name
3. The matched rule determines the type of the pattern variable

### Example from P Language

In `p.pg`:
```
P = '(' 'lambda' '(' Id ')' E ')' ;
E = '0' | '1' | Id | '(' 'if0' E E E ')' | ... ;
Id = id;
```

Valid pattern variables:
- `$e`, `$e1`, `$e2`, `$expr` → all match rule `E`
- `$id`, `$id1`, `$id2`, `$identifier` → all match rule `Id`
- `$p`, `$p1`, `$program` → all match rule `P`

Example pattern matching:
```haskell
subst id [e|(fold $e1 $e2 (lambda ( $id1 $id2 ) $e3))|] i =
  let e1 = subst id e1 i
      e2 = subst id e2 i
      e3 = subst id e3 i
    in [e|(fold $e1 $e2 (lambda ( $id1 $id2 ) $e3))|]
```

## Java Pattern Matching

### Available Rule Shortcuts

The Java grammar creates shortcuts by lowercasing the first letter of each rule name:

| Rule Name | Shortcut | Example Variables |
|-----------|----------|-------------------|
| `Expression` | `expression` | `$expression`, `$expression1`, `$expr`, `$e` |
| `Statement` | `statement` | `$statement`, `$statement1`, `$stmt`, `$s` |
| `CompoundName` | `compoundName` | `$compoundName`, `$compoundName1`, `$cn` |
| `Modifier` | `modifier` | `$modifier`, `$modifier1`, `$mod`, `$m` |
| `Literal` | `literal` | `$literal`, `$literal1`, `$lit`, `$l` |
| `StatementBlock` | `statementBlock` | `$statementBlock`, `$statementBlock1`, `$sb` |

**Note**: Single-letter prefixes may be ambiguous if multiple rules start with the same letter.

### Why `$left + $right` Failed

When we tried:
```haskell
[expression| $left + $right |]
```

The error was: `Unknown shortcut for left`

This failed because:
1. The system looked for prefixes: "left", "lef", "le", "l"
2. None of these match any Java grammar rule name
3. There's no rule named "Left", "Lef", "Le", or "L"

### Correct Pattern Matching Syntax

To match a binary addition, use variables with valid prefixes:

```haskell
-- Option 1: Full rule name prefix
[expression| $expression1 + $expression2 |]

-- Option 2: Shortened prefix (must still match)
[expression| $expr1 + $expr2 |]

-- Option 3: Single letter (if unambiguous)
[expression| $e1 + $e2 |]
```

Example:
```haskell
let testExpr = [expression| x + y |]
case testExpr of
    [expression| $expression1 + $expression2 |] -> do
        putStrLn $ "Left: " ++ show expression1
        putStrLn $ "Right: " ++ show expression2
    _ -> putStrLn "No match"
```

## Implementation Details

### Shortcut Resolution Algorithm

From `GenQ.hs` (lines 42-51):

```haskell
replaceAllPatterns1 str =
  let (pre, match, post) = str =~ qqPattern :: (String, String, String)
  in if match == ""
      then pre
      else let varName = init $ tail match  -- Remove $ and trailing char
               ruleVariants = catMaybes $ map (\ prefix -> M.lookup prefix qqShortcuts)
                                              $ reverse $ inits varName
               rule = case ruleVariants of
                        [] -> error $ "Unknown shortcut for " ++ varName
                        (rule : _) -> rule
           in pre ++ ('$' : rule ++ ":") ++ varName ++ ...
```

The algorithm:
1. Extracts variable name from `$varName`
2. Generates all prefixes in reverse order: ["varName", "varNam", ..., "v"]
3. Looks up each prefix in the shortcuts map
4. Uses the first (longest) matching prefix
5. If no match found, throws "Unknown shortcut" error

### Shortcuts Map

Generated shortcuts map (from `GenQ.hs` lines 142-148):

```haskell
shortCutTypes = map (\SyntaxRuleGroup { getSDataTypeName = typeName@(s : rest)} ->
                      ((C.toLower s : rest), typeName))
                  rulesWithoutProxies

qqShortcuts = M.fromList [ ("expression","Expression"),
                          ("statement","Statement"),
                          ("compoundName","CompoundName"),
                          ... ]
```

## Best Practices

1. **Use descriptive prefixes**: `$expression1`, `$expression2` is clearer than `$e1`, `$e2`

2. **Avoid ambiguous single letters**: If multiple rules start with the same letter (e.g., Statement, StatementBlock), use longer prefixes

3. **Be consistent**: Use the same prefix style throughout your code

4. **Document non-obvious prefixes**: If using shortened forms like `$expr`, add comments

## Testing Pattern Matching

To test if a pattern variable name works:
1. Check if any prefix of the variable name matches a rule name (case-insensitive)
2. The longest matching prefix determines the type

Example test file: `/home/user/rtk/test-grammars/java-qq-pattern-test.hs`

## Summary

Pattern matching with quasi-quotations **does work** for Java, but requires proper variable naming:

❌ **Wrong**: `$left + $right` (no rule matches "left")
✅ **Right**: `$expression1 + $expression2` (matches "Expression" rule)
✅ **Right**: `$expr1 + $expr2` (matches "Expression" rule)
✅ **Right**: `$e1 + $e2` (matches "Expression" rule if unambiguous)

The error "Unknown shortcut for left" simply means the variable name doesn't start with a valid rule name prefix.
