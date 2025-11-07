# RTK Bootstrap Self-Hosting

## Overview

This document describes RTK's **self-hosting** capability - the ability to use RTK's own generated parsers instead of hand-written Alex and Happy specifications.

Similar to how C compilers were eventually rewritten in C, RTK can use its own generated code to parse RTK grammar files.

## Current State

RTK currently uses:
- **Hand-written** `Lexer.x` (Alex specification) - 141 lines
- **Hand-written** `Parser.y` (Happy specification) - 206 lines

But RTK includes:
- `test-grammars/grammar.pg` - RTK's own grammar definition (81 lines)

This grammar file can generate equivalent lexer and parser code automatically.

## Bootstrap Build System

RTK includes a **multi-stage bootstrap build system** to support self-hosting development.

### Quick Start

```bash
# Generate parsers from grammar.pg
make generate-bootstrap

# Full bootstrap build and test
./build-bootstrap.sh

# Test self-hosting capability
make test-self-hosting

# Clean generated files
make clean-generated
```

### Build Stages

**Stage 0: Hand-Written Parser** (Always available)
- Uses `Lexer.x` and `Parser.y`
- Stable, checked into repository
- `make build` - Build with hand-written parser

**Stage 1: Generate from grammar.pg**
- Uses Stage 0 to generate parsers
- Creates files in `src/generated/` (NOT in git)
- `make generate-bootstrap` - Generate and compile

**Stage 2: Build with Generated Parser** (Experimental)
- Uses generated parsers from Stage 1
- Testing via `--use-generated` flag
- `make test-self-hosting` - Test self-hosting

### File Management

Generated files are **NOT checked into git**:
- `src/generated/Grammar*.{x,y,hs}` are in `.gitignore`
- Exception: Stub modules (`GrammarLexer.hs`, `GrammarParser.hs`) ARE tracked to allow CI builds
- Regenerated automatically from `grammar.pg`
- Clean history, no merge conflicts

## Design Rationale

### Industry Solutions for Bootstrapping

When building a self-hosting compiler, there are several approaches for managing generated files:

#### **Option 1: Bootstrap Binary (GCC, Rust, Go)**

**How it works:**
- Keep a pre-built binary (or download from releases)
- Use this "bootstrap compiler" to build the current version
- The bootstrap binary is version N-1, builds version N

**Pros:**
- ✅ No generated files in repo
- ✅ Always builds from source
- ✅ Clean git history

**Cons:**
- ❌ Requires maintaining bootstrap binaries
- ❌ Complex for new contributors
- ❌ Need separate download/distribution

#### **Option 2: Two-Stage Build (Bison, Happy, ANTLR)**

**How it works:**
- Keep "Stage 0" (hand-written parser) in repo
- Build system auto-generates from grammar if needed
- Generated files are in `.gitignore`

**Pros:**
- ✅ Hand-written parser always works (fallback)
- ✅ Generated files not in repo
- ✅ Automatic regeneration
- ✅ Simple for contributors

**Cons:**
- ⚠️ Must maintain both versions during transition

#### **Option 3: Checked-In Generated Files (OCaml)**

**How it works:**
- Generated files ARE checked into repo
- Marked as "generated" in git attributes
- Regenerate and commit when grammar changes

**Pros:**
- ✅ No build dependencies needed
- ✅ Works immediately after clone
- ✅ Reviewable in diffs

**Cons:**
- ❌ Generated files in git (noise)
- ❌ Can get out of sync with source
- ❌ Merge conflicts in generated code

### RTK's Choice: Two-Stage Build

RTK uses **Option 2 (Two-Stage Build)** because:

1. **Simplicity**: Contributors can `git clone` and `make build` immediately
2. **Clean git**: No generated files or binaries in repository
3. **Gradual transition**: Hand-written and generated parsers coexist
4. **Standard practice**: Matches Bison, Happy, and other parser generators

### Comparison Table

| Project | Approach | Bootstrap Method |
|---------|----------|-----------------|
| **GCC** | Option 1 | Bootstrap binary (gcc N-1 builds gcc N) |
| **Rust** | Option 1 | Bootstrap binary (rustc N-1 builds rustc N) |
| **OCaml** | Option 3 | Generated files in repo |
| **Bison** | Option 2 | Hand-written + auto-generate |
| **Happy** | Option 2 | Hand-written + auto-generate |
| **ANTLR** | Option 1 + 3 | Bootstrap binary + generated in repo |
| **RTK** | **Option 2** | **Hand-written + auto-generate** ✅ |

## Running the Self-Hosting Test

### Locally

```bash
# Build RTK and generate grammar files
make build
make test-grammar

# Run self-hosting test
make test-self-hosting
```

The self-hosting test:
1. Generates parsers using hand-written parser (Stage 0)
2. Compiles generated parsers (alex, happy)
3. Tests generated parser on `grammar.pg`
4. Compares output with hand-written parser
5. Validates that both produce **identical** results

### In CI

The test runs automatically in GitHub Actions. It validates that:
- Bootstrap generation succeeds
- Generated parser produces identical output to hand-written parser
- Self-hosting capability remains functional

## Implementation Details

### Directory Structure

```
rtk/
├── Lexer.x                        # Hand-written (Stage 0)
├── Parser.y                       # Hand-written (Stage 0)
├── test-grammars/grammar.pg       # RTK's own grammar
├── src/generated/
│   ├── README.md                  # Documentation
│   ├── ASTAdapter.hs              # Hand-written adapter
│   ├── GrammarLexer.hs            # Stub (tracked for CI)
│   ├── GrammarParser.hs           # Stub (tracked for CI)
│   ├── GrammarLexer.x             # Generated (ignored)
│   ├── GrammarParser.y            # Generated (ignored)
│   └── GrammarQQ.hs               # Generated (ignored)
└── build-bootstrap.sh             # Convenience script
```

### Stub Modules

To enable CI builds without running `make generate-bootstrap` first:
- Minimal stub versions of `GrammarLexer.hs` and `GrammarParser.hs` are tracked in git
- Stubs provide type definitions but error if functions are called
- Real generated modules replace stubs when `make generate-bootstrap` runs
- This is "type-complete but value-stub" pattern

### Makefile Targets

```makefile
# Default: build with hand-written parser (Stage 0)
build:
    cabal build

# Generate parser files from grammar.pg (Stage 1)
generate-bootstrap: build
    ./rtk test-grammars/grammar.pg src/generated/
    cabal exec alex -- src/generated/GrammarLexer.x -o src/generated/GrammarLexer.hs
    cabal exec happy -- src/generated/GrammarParser.y -o src/generated/GrammarParser.hs

# Test self-hosting capability
test-self-hosting: generate-bootstrap
    # Test both parsers produce identical output
    ./rtk test-grammars/grammar.pg test-out/handwritten-test
    ./rtk --use-generated test-grammars/grammar.pg test-out/generated-test
    diff -q test-out/handwritten-test/GrammarParser.y test-out/generated-test/GrammarParser.y
    diff -q test-out/handwritten-test/GrammarQQ.hs test-out/generated-test/GrammarQQ.hs

# Clean generated files
clean-generated:
    rm -f src/generated/Grammar*.{x,y,hs,hi,o}
```

## Developer Workflow

### First Time Setup

```bash
git clone https://github.com/prozak/rtk
cd rtk
make build                   # Uses hand-written parser (always works)
make generate-bootstrap      # Generate from grammar.pg (optional)
```

### Daily Development

```bash
# Edit hand-written parser
vim Lexer.x
make build
make test

# Edit grammar.pg (for self-hosting work)
vim test-grammars/grammar.pg
make generate-bootstrap      # Regenerate
make test-self-hosting       # Validate
```

### Testing Self-Hosting

```bash
# Generate and test
make generate-bootstrap

# Test generated parser
cabal run rtk -- --use-generated test-grammars/java.pg test-out
```

## CI/CD Strategy

GitHub Actions runs both standard and bootstrap builds:

```yaml
# Standard build (hand-written parser)
- name: Run all tests
  run: make test

# Bootstrap build (generated parser)
- name: Bootstrap Generation
  run: make generate-bootstrap

# Self-hosting validation
- name: Self-Hosting Test
  run: make test-self-hosting
```

The CI validates:
1. Hand-written parser works
2. Bootstrap generation succeeds
3. Generated parser produces identical output
4. Self-hosting capability is functional

## Migration Path

### **Phase 1: Development (Current)**
- Hand-written parser is primary
- Generated parser is experimental (`--use-generated`)
- Both coexist, hand-written is default
- Generated files NOT in git (except stubs)

### **Phase 2: Validation (Later)**
- Both parsers produce identical output
- Self-hosting test passes in CI
- Generated parser considered stable

### **Phase 3: Transition (Future)**
- Make generated parser the default
- Keep hand-written as fallback
- Update documentation

### **Phase 4: Fully Self-Hosted (Eventually)**
- Remove hand-written parser
- Keep it in git history as reference
- Generated parser is the only parser

## Path to Full Self-Hosting

To achieve full self-hosting:

1. **Verify grammar completeness**: Ensure `test-grammars/grammar.pg` captures all RTK features
2. **Test equivalence**: Verify generated parsers handle all test cases
3. **Update imports**: Switch `main.hs` to use generated parsers by default
4. **Bootstrap cycle**:
   - Use RTK v1 (hand-written) → Generate RTK v2 (generated)
   - Use RTK v2 → Generate RTK v3
   - Verify v2 ≡ v3 (stable point)
5. **Retire hand-written files**: Keep as reference but use generated versions

## Benefits of Self-Hosting

- **Dogfooding**: RTK uses its own capabilities
- **Simplified maintenance**: Update `grammar.pg` instead of editing `.x`/`.y` files
- **QuasiQuoter support**: Get compile-time code generation for grammar manipulation
- **Feature validation**: Every RTK feature must work on RTK's own grammar
- **Clean evolution**: Grammar changes automatically reflected in parser

## Files

- `build-bootstrap.sh` - Convenience script for full bootstrap build
- `test-grammars/grammar.pg` - RTK's grammar definition
- `Lexer.x` - Hand-written lexer (Stage 0)
- `Parser.y` - Hand-written parser (Stage 0)
- `src/generated/ASTAdapter.hs` - Converts generated AST to hand-written AST
- `src/generated/GrammarLexer.hs` - Stub lexer (replaced when generated)
- `src/generated/GrammarParser.hs` - Stub parser (replaced when generated)
- `src/generated/README.md` - Generated files documentation
- `.github/workflows/ci.yml` - CI configuration
- `makefile` - Build system with bootstrap targets

## Further Reading

- [Bootstrapping Compilers](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
- Self-hosting examples: GCC, Rust compiler, TypeScript, Bison
