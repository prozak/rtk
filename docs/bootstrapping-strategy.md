# Bootstrapping Strategy: Managing Generated Files

## The Problem

**Question:** How do we handle generated parser files when:
1. We want to always build from the latest `grammar.pg`
2. We don't want to check generated files into the repository
3. Building RTK requires RTK to generate parsers from `grammar.pg`

This is the classic **bootstrapping problem** in self-hosting compilers.

---

## Industry Solutions

### **Option 1: Bootstrap Binary (GCC, Rust, Go)**

**How it works:**
- Keep a pre-built binary (or download from releases)
- Use this "bootstrap compiler" to build the current version
- The bootstrap binary is version N-1, builds version N

**Example: Rust**
```bash
# Stage 0: Download pre-built bootstrap rustc (version 1.70)
# Stage 1: Use bootstrap rustc to compile current rustc (version 1.71)
# Stage 2: Use stage1 rustc to recompile itself (verification)
```

**Pros:**
- ✅ No generated files in repo
- ✅ Always builds from source
- ✅ Clean git history

**Cons:**
- ❌ Requires maintaining bootstrap binaries
- ❌ Complex for new contributors
- ❌ Need separate download/distribution

---

### **Option 2: Two-Stage Build (Most Common)**

**How it works:**
- Keep "Stage 0" (hand-written parser) in repo
- Build system auto-generates from `grammar.pg` if needed
- Generated files are in `.gitignore`

**Example: Parser Generators (Bison, ANTLR, Happy)**
```bash
# Stage 0: Hand-written parser (always available)
make build         # Uses hand-written parser
make regenerate    # Generates from grammar.pg using Stage 0
make build-gen     # Builds with generated parser (optional)
```

**Pros:**
- ✅ Hand-written parser always works (fallback)
- ✅ Generated files not in repo
- ✅ Automatic regeneration
- ✅ Simple for contributors

**Cons:**
- ⚠️ Must maintain both hand-written and generated versions during transition

---

### **Option 3: Checked-In Generated Files (OCaml, Some Haskell)**

**How it works:**
- Generated files ARE checked into repo
- Marked as "generated" in git attributes
- Regenerate and commit when grammar changes

**Example: Many parser-based projects**
```bash
# Generated files in repo:
src/Parser.hs        # Generated from Parser.y
src/Lexer.hs         # Generated from Lexer.x

# .gitattributes:
*.hs linguist-generated=true
```

**Pros:**
- ✅ No build dependencies needed
- ✅ Works immediately after clone
- ✅ Reviewable in diffs

**Cons:**
- ❌ Generated files in git (noise)
- ❌ Can get out of sync with source
- ❌ Merge conflicts in generated code

---

### **Option 4: Makefile Automation (Pragmatic)**

**How it works:**
- Makefile knows dependencies
- Auto-regenerates when `grammar.pg` changes
- Generated files in build directories, not tracked

**Example:**
```makefile
# If grammar.pg changes, regenerate
src/generated/GrammarParser.y: test-grammars/grammar.pg rtk
    ./rtk test-grammars/grammar.pg src/generated/

# If .y changes, recompile
src/generated/GrammarParser.hs: src/generated/GrammarParser.y
    happy src/generated/GrammarParser.y -o src/generated/GrammarParser.hs
```

**Pros:**
- ✅ Automatic dependency tracking
- ✅ Regenerates only when needed
- ✅ Clean git history

**Cons:**
- ⚠️ Requires make
- ⚠️ Can be complex with cabal

---

## **Recommended for RTK: Hybrid Two-Stage**

I recommend **Option 2 (Two-Stage)** with automation:

### **Architecture:**

```
┌─────────────────────────────────────────────────────────┐
│ STAGE 0: Hand-Written Parser (Always in Repo)         │
├─────────────────────────────────────────────────────────┤
│  Lexer.x          (hand-written, stable)               │
│  Parser.y         (hand-written, stable)               │
│  TokenProcessing  (shared by both stages)              │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
             Build rtk (uses hand-written)
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│ STAGE 1: Generate from grammar.pg                      │
├─────────────────────────────────────────────────────────┤
│  ./rtk test-grammars/grammar.pg src/generated/         │
│                                                         │
│  Creates:                                               │
│    src/generated/GrammarLexer.x   (NOT in git)        │
│    src/generated/GrammarParser.y  (NOT in git)        │
│    src/generated/GrammarQQ.hs     (NOT in git)        │
└─────────────────────────────────────────────────────────┘
                          │
                          ▼
             Compile generated files
                          │
                          ▼
┌─────────────────────────────────────────────────────────┐
│ STAGE 2: Build with Generated Parser (Optional)       │
├─────────────────────────────────────────────────────────┤
│  ./rtk --use-generated (uses generated parser)         │
│                                                         │
│  For testing/validation only                           │
└─────────────────────────────────────────────────────────┘
```

### **Implementation:**

#### **1. .gitignore**
```gitignore
# Generated parser files (regenerated from grammar.pg)
src/generated/GrammarLexer.x
src/generated/GrammarParser.y
src/generated/GrammarQQ.hs
src/generated/GrammarLexer.hs
src/generated/GrammarParser.hs
src/generated/*.hi
src/generated/*.o

# Keep only:
# - src/generated/README.md
# - src/generated/ASTAdapter.hs
```

#### **2. Makefile Targets**
```makefile
# Default: build with hand-written parser (Stage 0)
.PHONY: build
build:
    cabal build

# Generate parser files from grammar.pg (Stage 1)
.PHONY: generate-bootstrap
generate-bootstrap: build
    ./rtk test-grammars/grammar.pg src/generated/
    cd src/generated && alex GrammarLexer.x -o GrammarLexer.hs
    cd src/generated && happy GrammarParser.y -o GrammarParser.hs

# Build with generated parser (Stage 2) - experimental
.PHONY: build-bootstrap
build-bootstrap: generate-bootstrap
    cabal build

# Clean generated files
.PHONY: clean-generated
clean-generated:
    rm -f src/generated/Grammar*.{x,y,hs,hi,o}
```

#### **3. Cabal Configuration**
```cabal
-- Two build modes

-- Default executable: uses hand-written parser
Executable rtk
  Main-is: main.hs
  Build-Depends: base, ...
  Other-Modules:
    Lexer,              -- Hand-written
    Parser,             -- Hand-written
    TokenProcessing,    -- Shared
    ...

-- Flag to enable generated parser
Flag use-generated
  Description: Use generated parser from grammar.pg
  Default: False
  Manual: True

-- Conditional modules based on flag
if flag(use-generated)
  Other-Modules:
    GrammarLexer,       -- Generated
    GrammarParser,      -- Generated
    GrammarQQ,          -- Generated
    ASTAdapter
```

#### **4. Build Script (Convenience)**
```bash
#!/bin/bash
# build-bootstrap.sh

set -e

echo "=== RTK Bootstrap Build ==="
echo ""

# Stage 0: Build with hand-written parser
echo "Stage 0: Building RTK with hand-written parser..."
cabal build
echo "✓ Stage 0 complete"
echo ""

# Stage 1: Generate parser from grammar.pg
echo "Stage 1: Generating parser from grammar.pg..."
cabal run rtk -- test-grammars/grammar.pg src/generated/

echo "Stage 1: Compiling generated files..."
cd src/generated
cabal exec alex -- GrammarLexer.x -o GrammarLexer.hs
cabal exec happy -- GrammarParser.y -o GrammarParser.hs
cd ../..
echo "✓ Stage 1 complete"
echo ""

# Stage 2: Build with generated parser (optional)
echo "Stage 2: Building RTK with generated parser..."
cabal build -f use-generated
echo "✓ Stage 2 complete"
echo ""

echo "=== Bootstrap build successful! ==="
echo ""
echo "Test with:"
echo "  cabal run rtk -- --use-generated test-grammars/grammar.pg test-out"
```

---

## **Developer Workflow**

### **First Time Setup:**
```bash
git clone https://github.com/prozak/rtk
cd rtk
make build                   # Uses hand-written parser (always works)
make generate-bootstrap      # Generate from grammar.pg (optional)
```

### **Daily Development:**
```bash
# Edit hand-written parser
vim Lexer.x
make build
make test

# Edit grammar.pg (for self-hosting work)
vim test-grammars/grammar.pg
make generate-bootstrap      # Regenerate
make test-bootstrap         # Compare outputs
```

### **Testing Self-Hosting:**
```bash
# Generate and test
make generate-bootstrap
cabal run rtk -- --use-generated test-grammars/java.pg test-out
```

---

## **CI/CD Strategy**

### **GitHub Actions:**
```yaml
jobs:
  # Job 1: Standard build (hand-written parser)
  build-handwritten:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Build with hand-written parser
        run: make build
      - name: Run tests
        run: make test-all-java

  # Job 2: Bootstrap build (generated parser)
  build-bootstrap:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4
      - name: Build stage 0
        run: make build
      - name: Generate from grammar.pg
        run: make generate-bootstrap
      - name: Test bootstrap
        run: make test-bootstrap
      - name: Compare outputs
        run: |
          # Verify hand-written and generated produce same results
          ./rtk test-grammars/grammar.pg test-out/hand
          ./rtk --use-generated test-grammars/grammar.pg test-out/gen
          diff -r test-out/hand test-out/gen
```

---

## **Migration Path**

### **Phase 1: Development (Now)**
- Hand-written parser is primary
- Generated parser is experimental (`--use-generated`)
- Both coexist, hand-written is default
- Generated files NOT in git

### **Phase 2: Validation (Later)**
- Both parsers produce identical output
- Bootstrap test passes in CI
- Generated parser considered stable

### **Phase 3: Transition (Future)**
- Make generated parser the default
- Keep hand-written as fallback
- Update docs

### **Phase 4: Fully Self-Hosted (Eventually)**
- Remove hand-written parser
- Keep it in git history as reference
- Generated parser is the only parser

---

## **Comparison to Other Projects**

| Project | Approach | Bootstrap Method |
|---------|----------|-----------------|
| **GCC** | Option 1 | Bootstrap binary (gcc N-1 builds gcc N) |
| **Rust** | Option 1 | Bootstrap binary (rustc N-1 builds rustc N) |
| **OCaml** | Option 3 | Generated files in repo |
| **Bison** | Option 2 | Hand-written + auto-generate |
| **Happy** | Option 2 | Hand-written + auto-generate |
| **ANTLR** | Option 1 + 3 | Bootstrap binary + generated in repo |
| **RTK** | **Option 2** | **Hand-written + auto-generate** ✅ |

---

## **Recommendation Summary**

**For RTK, use Option 2 (Two-Stage Build):**

✅ **Pros:**
- Hand-written parser always works (no download needed)
- Generated files not in git (clean history)
- Automatic regeneration via Makefile
- Gradual migration path
- Simple for contributors

✅ **Implementation:**
1. Add generated files to `.gitignore`
2. Add `generate-bootstrap` Makefile target
3. Add build script for convenience
4. Update CI to test both modes
5. Document in BOOTSTRAP.md

✅ **Philosophy:**
- Stage 0 (hand-written) = **stable, always works**
- Stage 1 (generated) = **experimental, opt-in**
- Gradual transition over time
- Both coexist during development

This matches how most parser generators handle bootstrapping while maintaining RTK's goal of self-hosting.
