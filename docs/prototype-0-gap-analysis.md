# Prototype 0: Gap Analysis

## Executive Summary

Comparison of hand-written `Lexer.x` (141 lines) and `Parser.y` (206 lines) with what RTK will generate from `test-grammars/grammar.pg` (81 lines).

**Bottom Line:** The grammar.pg is **semantically complete** but will generate code with significant **structural differences**. Main gaps are in custom Haskell code, not grammar rules.

---

## Lexer Comparison (Lexer.x vs grammar.pg)

### ✅ What Matches (Grammar Coverage)

| Feature | grammar.pg | Lexer.x | Status |
|---------|-----------|---------|--------|
| Keywords | `'grammar'`, `'imports'` | Lines 24-25 | ✅ Match |
| Operators | `'='`, `';'`, `':'`, `'|'`, etc. | Lines 28-39 | ✅ Match |
| Annotations | `'@shortcuts'`, `'@symmacro'` | Lines 26-27 | ✅ Match |
| Identifiers | `[a-zA-Z][A-Za-z0-9_]*` | Lines 44, 70 | ✅ Match |
| String literals | `'\'' ... '\''` | Lines 40, 71 | ✅ Match |
| Regex literals | `'[' ... ']'` | Lines 41, 77 | ✅ Match |
| Triple-quoted strings | `"""..."""` | Lines 45, 76 | ✅ Match |
| Comments (simple) | `'#' .* [\n]` | Line 19 | ✅ Match |
| Whitespace | `[ \t\n]+` | Line 18, 78 | ✅ Match |

### ⚠️ Differences (Implementation Details)

| Feature | Hand-written | Generated | Impact |
|---------|-------------|-----------|---------|
| **Nested comments** | Multi-state (lines 20-23, 53-72) | Single regex (line 80) | 🟡 Functional difference |
| **Alex wrapper** | `%wrapper "monadUserState"` | Will use basic wrapper | 🟢 Internal only |
| **Token concatenation** | `catBigstrs` (lines 125-130) | Not generated | 🟡 May need workaround |
| **Backslash handling** | `unBackQuote` (lines 103-109) | Not generated | 🟡 May need workaround |
| **Error messages** | Custom (line 134) | Default Alex errors | 🟢 Acceptable |
| **Module header** | Custom imports (line 2) | Auto-generated | 🟢 Internal only |

#### Nested Comments Detail

**Hand-written (lines 20-23):**
```alex
<0>"/*"             { beginMultiLineComment }
<mlcomment> "/*"    { beginMultiLineComment }
<mlcomment> "*/"    { tryEndMultiLineComment }
<mlcomment>([^\*\/]|[\*][^\/]|[\/][^\*])* { skip }
```
- Uses Alex start codes (`<0>`, `<mlcomment>`)
- Maintains comment depth counter in user state
- Properly handles `/* /* nested */ */`

**Grammar.pg (line 80):**
```
Ignore: comment1 = '/*' ([^\*]|[\*][^\/]|[\n])* '*/';
```
- Single regex pattern
- **Cannot handle nesting**: `/* /* this breaks */ */`
- Simpler but less correct

**Decision:** Accept this limitation per strategy (pragmatic trade-off)

---

## Parser Comparison (Parser.y vs grammar.pg)

### ✅ Grammar Rules Match

All parser rules in grammar.pg (lines 18-62) correspond to Parser.y rules:

| grammar.pg | Parser.y | Match |
|------------|----------|-------|
| `Grammar = 'grammar' StrLit ';' ImportsOpt RuleList` | Line 43 | ✅ |
| `ImportsOpt = ('imports' bigstr)?` | Lines 45-46 | ✅ |
| `RuleList = Rule *` | Lines 48-50 | ✅ |
| `Rule = Name '=' Clause ';'` (and variants) | Lines 67-70 | ✅ |
| `Clause` alternatives | Lines 72-99 | ✅ |

### 🔴 Major Difference: Semantic Actions

**Hand-written Parser.y has explicit semantic actions:**

```haskell
Grammar : grammar str ';' ImportsOpt Rules
        { InitialGrammar $2 $4 (reverse $5) }
        --  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^
```

**Every rule** (lines 43-99) has a semantic action that:
- Constructs specific AST nodes (`InitialGrammar`, `IRule`, `IClause`, etc.)
- Manipulates data (`reverse $5`, `$2 : $1`, etc.)
- Uses predefined types from lines 107-205

**grammar.pg has NO semantic actions:**
```
Grammar = 'grammar' StrLit ';' ImportsOpt RuleList ;
```

**What RTK will generate:**
- Auto-generated AST types based on rule structure
- Automatic constructors
- **Different type names and field names**

**Impact:** 🔴 **High** - Generated AST will not match hand-written AST

---

## AST Structure Differences

### Hand-Written AST (Parser.y lines 107-205)

```haskell
data InitialGrammar = InitialGrammar
  { getIGrammarName :: String
  , getImports :: String
  , getIRules :: [IRule]
  }

data IRule = IRule
  { getIDataTypeName :: Maybe String
  , getIDataFunc :: Maybe String
  , getIRuleName :: String
  , getIClause :: IClause
  , getIRuleOptions :: [IOption]
  }

data IClause = IId String
             | IStrLit String
             | IDot
             | IRegExpLit String
             | IStar IClause (Maybe IClause)
             | IPlus IClause (Maybe IClause)
             | IAlt [IClause]
             | ISeq [IClause]
             | IOpt IClause
             | ILifted IClause
             | IIgnore IClause
```

**Key features:**
- Explicit field accessors (`getIGrammarName`, etc.)
- Specific constructor names
- Nested Maybe types
- Helper types (`GrammarInfo`, `AntiRule`, etc.)
- Derives `Typeable`, `Data` for generic programming

### Generated AST (Expected)

RTK will generate something like:

```haskell
data Grammar = Grammar StrLit ImportsOpt RuleList

data Rule = Rule_1 Name Clause
          | Rule_2 Name Name Clause
          | Rule_3 Name Name Name Clause
          | Rule_4 Name Name Clause

data Clause = Clause_Alt Clause Clause2
            | Clause_Seq Clause2 Clause3
            -- etc.
```

**Differences:**
- Auto-generated constructor names (`Grammar`, `Rule_1`, etc.)
- Positional fields, no accessors
- Different nesting structure
- May have different type hierarchy

**Impact:** 🔴 **Critical** - Entire codebase uses hand-written AST types

---

## Custom Haskell Code That Won't Be Generated

### Lexer.x Custom Code (lines 48-141)

1. **Comment state machine** (lines 50-76):
   - `getStateCommentDepth`, `setStateCommentDepth`
   - `beginMultiLineComment`, `tryEndMultiLineComment`
   - `AlexUserState` with comment depth
   - **93 lines of custom logic**

2. **Token processing** (lines 103-139):
   - `unBackQuote` - backslash escape handling
   - `alexScanTokens` - main entry point
   - `catBigstrs` - concatenate adjacent BigStr tokens
   - `rtkError` - custom error messages
   - `simple`, `simple1` - token constructors

3. **Token data type** (lines 78-101):
   - 19 token constructors
   - Derives `Eq`, `Show`

### Parser.y Custom Code (lines 1-11, 101-206)

1. **Module header** (lines 1-10):
   - Imports: `Data.Generics`, `Data.Data`, `Data.Map`, `Data.Set`
   - Required for generic programming

2. **Semantic actions** (lines 43-100):
   - Every rule has custom Haskell code
   - Data manipulation (`reverse`, list cons, etc.)
   - Constructor selection

3. **AST definitions** (lines 107-205):
   - 11 data types
   - Helper functions (`addRuleOptions`, `isLexicalRule`, etc.)
   - Complex types with Maybe, lists, maps

4. **Error handling** (lines 103-105):
   - Custom `parseError` with context

---

## QuasiQuoter Generation

**What exists:** None (hand-written parsers don't generate QQ)

**What will be generated:** `GrammarQQ.hs` from grammar.pg

**Expected output:**
```haskell
module GrammarQQ where

import Language.Haskell.TH.Quote

grammar :: QuasiQuoter
grammar = QuasiQuoter { ... }

rule :: QuasiQuoter
rule = QuasiQuoter { ... }

-- etc for each top-level rule
```

**Status:** 🟢 **New capability** - This is a benefit, not a gap!

---

## Quantified Gap Summary

| Category | Lines Hand-Written | Lines in grammar.pg | Gap Type |
|----------|-------------------|---------------------|----------|
| **Lexer rules** | 29 (lines 16-45) | 14 (lines 63-81) | 🟢 Equivalent |
| **Lexer custom code** | 93 (lines 48-141) | 0 | 🟡 Feature differences |
| **Parser rules** | 57 (lines 43-100) | 44 (lines 18-62) | 🟢 Equivalent |
| **Parser AST types** | 99 (lines 107-205) | 0 (auto-gen) | 🔴 Structural difference |
| **Total** | 347 lines | 81 lines | Different paradigms |

---

## Critical Path to Self-Hosting

### Blockers (Must Address)

1. **AST Incompatibility** 🔴
   - Generated AST ≠ hand-written AST
   - **Solution:** Create `ASTAdapter.hs` to convert between types
   - **Alternative:** Refactor entire codebase to use generated types
   - **Effort:** Medium (adapter) or High (refactor)

2. **Custom Token Processing** 🟡
   - `catBigstrs`, `unBackQuote` not in generated code
   - **Solution:** Add as post-processing or extend GenX.hs
   - **Effort:** Low-Medium

### Acceptable Differences (Per Strategy)

1. **Nested Comments** 🟢
   - Documented limitation
   - Simple regex is good enough
   - Can add later if critical

2. **Error Messages** 🟢
   - Default Alex/Happy errors acceptable
   - Can improve later

3. **Module Structure** 🟢
   - Generated imports/headers fine
   - Internal implementation detail

---

## Recommendations for Prototype 1

### Immediate Actions

1. **Add dual-mode flag** to `main.hs`:
   ```haskell
   if "--use-generated" `elem` args
     then useGeneratedParser
     else useHandWrittenParser
   ```

2. **Generate files** and see actual output:
   ```bash
   make test-grammar  # Creates test-out/Grammar*.{x,y,hs}
   ```

3. **Create stub adapter**:
   ```haskell
   module ASTAdapter where
   import qualified GrammarAST as Gen
   import qualified Parser as Hand

   -- Stub - will fill in based on actual generated types
   convert :: Gen.Grammar -> Hand.InitialGrammar
   convert = undefined
   ```

### Validation Tests

1. **Compile test:** Does generated code compile with GHC?
2. **Parse test:** Can generated parser parse grammar.pg?
3. **AST test:** What does generated AST look like?
4. **QQ test:** Does GrammarQQ.hs compile?

---

## Timeline Estimate

Based on gap analysis:

- **Prototype 1** (dual-mode): 2-3 hours ✅ (minimal code changes)
- **Prototype 2** (close loop): 1-2 days 🟡 (AST adapter complexity)
- **Prototype 3** (QQ validation): 4-6 hours ✅ (should work out of box)
- **Prototype 4** (bootstrap cycle): 1 day 🟡 (depends on Prototype 2)
- **Prototype 5** (full testing): 2-3 days 🟡 (bug fixing)

**Total:** 5-7 days of focused work

---

## Open Questions

1. **AST Strategy:** Adapter vs full refactor?
2. **Token processing:** Extend GenX.hs or post-process?
3. **Success bar:** Functional equivalence or exact equivalence?
4. **Migration plan:** When to switch to generated as default?

---

## Next Steps

1. ✅ Run `make test-grammar` to generate actual files
2. ✅ Inspect generated `GrammarLexer.x`, `GrammarParser.y`, `GrammarQQ.hs`
3. ✅ Compare actual output vs this analysis
4. ✅ Update this document with real differences
5. ✅ Proceed to Prototype 1 implementation

---

**Analysis Date:** 2025-11-02
**Analyzer:** Claude
**Status:** Based on code inspection, not actual generation (cabal not available)
**Next:** Need to run actual generation to validate predictions
