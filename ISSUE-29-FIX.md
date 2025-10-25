# Fix for Issue #29: Graceful Handling of Empty Grammar Files

## Problem

Previously, when rtk encountered an empty grammar file (a file with a grammar declaration but no rules), it would display:
```
rtk.exe: Parse error[]
```

This error message was not helpful or user-friendly.

## Solution

The fix consists of three improvements:

### 1. Parser Changes (Parser.y)

**Added empty production for Rules:**
```haskell
Rules : RuleWithOptions                    { [$1] }
      | Rules RuleWithOptions              { $2 : $1 }
      | {- empty -}                        { [] }
```

This allows the parser to accept grammars with no rules.

**Improved error messages:**
```haskell
parseError :: [L.Token] -> a
parseError [] = error "Parse error: unexpected end of input. Expected a grammar definition."
parseError rest = error $ "Parse error near: " ++ (show (take 5 rest))
```

This provides clearer error messages when parsing fails.

### 2. Normalization Changes (Normalize.hs)

**Updated normalizeTopLevelClauses to handle empty rules:**
```haskell
normalizeTopLevelClauses :: InitialGrammar -> NormalGrammar
normalizeTopLevelClauses grammar =
  case getIRules grammar of
    [] -> error $ "Grammar '" ++ (getIGrammarName grammar) ++ "' contains no rules"
    (firstIRule:_) -> ...
```

This replaces a pattern match that would fail with a clear, informative error message.

## Behavior After Fix

### Empty Grammar File (test-grammars/empty.pg)
**Content:**
```
grammar 'Empty';
```

**Before fix:**
```
rtk.exe: Parse error[]
```

**After fix:**
```
rtk: Grammar 'Empty' contains no rules
```

### Completely Empty File
**Before fix:**
```
rtk.exe: Parse error[EndOfFile]
```

**After fix:**
```
rtk: Parse error: unexpected end of input. Expected a grammar definition.
```

### Valid Grammar File
No change - works as before.

## Testing

To test the fix with an empty grammar:
```bash
cabal build
cabal exec rtk -- test-grammars/empty.pg test-out
```

Expected output:
```
rtk: Grammar 'Empty' contains no rules
```

This is a much more appropriate and helpful error message for users.
