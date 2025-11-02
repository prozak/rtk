# Prototype 2: Close the Loop - Implementation Plan

## Goal
Get the generated parser to successfully parse `test-grammars/grammar.pg` using RTK's `--use-generated` flag.

## Current Status
- ✅ Prototype 0: Gap analysis complete
- ✅ Prototype 1: Dual-mode flag infrastructure ready
- ✅ Token processing refactored for reuse
- 🔄 Prototype 2: In progress

## Implementation Steps

### Step 1: Generate Parser Files
```bash
# Use hand-written RTK to generate from grammar.pg
rtk test-grammars/grammar.pg src/generated/
```

This creates:
- `src/generated/GrammarLexer.x` - Alex lexer specification
- `src/generated/GrammarParser.y` - Happy parser specification
- `src/generated/GrammarQQ.hs` - QuasiQuoter module

### Step 2: Compile Generated Files
The generated `.x` and `.y` files need to be compiled to `.hs`:
```bash
cd src/generated
alex GrammarLexer.x -o GrammarLexer.hs
happy GrammarParser.y -o GrammarParser.hs
```

### Step 3: Analyze Generated AST
Look at `GrammarParser.y` to see:
- What data types are generated (e.g., `data Grammar = ...`)
- How they differ from hand-written `InitialGrammar`
- What fields/constructors exist

### Step 4: Create AST Adapter
Create `src/generated/ASTAdapter.hs` using **quasi-quotation** for elegant pattern matching:
```haskell
{-# LANGUAGE QuasiQuotes #-}
module ASTAdapter where

import qualified Parser as Hand
import qualified GrammarParser as Gen
import qualified GrammarQQ as QQ

-- Convert generated AST to hand-written AST using quasi-quotation
-- This demonstrates RTK using its own features (dogfooding!)
convertGrammar :: Gen.Grammar -> Hand.InitialGrammar
convertGrammar [QQ.grammar| grammar $name ';' $imports $rules |] =
    Hand.InitialGrammar
        { getIGrammarName = name
        , getImports = imports
        , getIRules = map convertRule rules
        }

-- Pattern match rules declaratively
convertRule [QQ.rule| $name = $clause ; |] = ...
convertRule [QQ.rule| $type : $name = $clause ; |] = ...
```

**Benefits:**
- Patterns read like grammar rules (self-documenting)
- Uses RTK's own quasi-quotation features
- Maintainable and type-safe

### Step 5: Integrate into main.hs
Update the `--use-generated` section:
```haskell
when (useGenerated opts) $ do
    -- Load grammar file
    content <- readFile (grammarFile opts)

    -- Use generated lexer
    rawTokens <- Gen.alexScanTokens content
    tokens <- processTokens rawTokens  -- Reuse token processing!

    -- Use generated parser
    genGrammar <- Gen.parse tokens

    -- Convert to hand-written AST
    grammar <- return $ convertGrammar genGrammar

    -- Continue with normal pipeline
    ...
```

### Step 6: Test
```bash
# Build with generated parsers
cabal build

# Test generated mode
rtk --use-generated test-grammars/grammar.pg test-out

# Verify it produces same output as hand-written mode
rtk test-grammars/grammar.pg test-out/hand
rtk --use-generated test-grammars/grammar.pg test-out/gen
diff -r test-out/hand test-out/gen
```

## Expected Challenges

### Challenge 1: AST Structure Differences
**Problem:** Generated AST will have different type names and structure

**Example:**
```haskell
-- Hand-written
data InitialGrammar = InitialGrammar
  { getIGrammarName :: String
  , getImports :: String
  , getIRules :: [IRule]
  }

-- Generated (expected)
data Grammar = Grammar StrLit ImportsOpt RuleList
```

**Solution:** Create adapter with pattern matching to extract values

### Challenge 2: Module Dependencies
**Problem:** Generated modules need to import Token from Lexer module

**Solution:** May need to extract Token type to separate module, or adjust generated code

### Challenge 3: Type Mismatches
**Problem:** Some helper types (GrammarInfo, AntiRule, etc.) won't exist in generated AST

**Solution:** Adapter creates these with default/empty values, fill in during normalization

## Success Criteria

✅ **Minimum:**
- `rtk --use-generated test-grammars/grammar.pg test-out` completes without error
- Generated files are created in test-out/

✅ **Ideal:**
- Generated output matches hand-written output (diff passes)
- All pipeline stages work with generated AST

## Timeline

- **Step 1-2:** Generate and compile files (30 mins)
- **Step 3:** Analyze AST structure (1 hour)
- **Step 4:** Create adapter (3-4 hours) ← Most complex
- **Step 5:** Integrate into main.hs (1-2 hours)
- **Step 6:** Test and debug (2-3 hours)

**Total:** ~1 day (as estimated in strategy)

## Next Prototype

After Prototype 2 succeeds:
- **Prototype 3:** Validate GrammarQQ.hs works for quasi-quotation
- **Prototype 4:** Bootstrap cycle (v1 → v2 → v3)
- **Prototype 5:** Full test suite with generated parser

## Files to Create/Modify

**New:**
- `src/generated/ASTAdapter.hs` - Conversion layer
- `src/generated/GrammarLexer.x` - Generated (by RTK)
- `src/generated/GrammarParser.y` - Generated (by RTK)
- `src/generated/GrammarQQ.hs` - Generated (by RTK)
- `src/generated/GrammarLexer.hs` - Compiled (by alex)
- `src/generated/GrammarParser.hs` - Compiled (by happy)

**Modified:**
- `main.hs` - Implement --use-generated logic
- `rtk.cabal` - Add generated modules to build
- `docs/self-hosting-strategy.md` - Update status

**Not Modified:**
- `Lexer.x` - Hand-written stays as-is
- `Parser.y` - Hand-written stays as-is
- All other existing code

## Notes

- This is experimental code - keep hand-written parsers as default
- Focus on making it work, not making it perfect
- Performance doesn't matter yet
- Error messages can be basic
- Accept any working adapter, even if inefficient
