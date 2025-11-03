# RTK Bootstrap Self-Hosting Test

## Overview

This test tracks RTK's progress toward **self-hosting** - the ability to use RTK's own generated parsers instead of hand-written Alex and Happy specifications.

Similar to how C compilers were eventually rewritten in C, RTK can potentially use its own generated code to parse RTK grammar files.

## Current State

RTK currently uses:
- **Hand-written** `Lexer.x` (Alex specification) - 141 lines
- **Hand-written** `Parser.y` (Happy specification) - 206 lines

But RTK includes:
- `test-grammars/grammar.pg` - RTK's own grammar definition (81 lines)

This grammar file can generate equivalent lexer and parser code automatically.

## Bootstrap Build System

RTK now includes a **multi-stage bootstrap build system** to support self-hosting development.

### Quick Start

```bash
# Generate parsers from grammar.pg
make generate-bootstrap

# Full bootstrap build and test
./build-bootstrap.sh

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
- Regenerated automatically from `grammar.pg`
- Clean history, no merge conflicts

See `docs/bootstrapping-strategy.md` for detailed explanation of the approach.

## Running the Test

### Locally

```bash
# Build RTK and generate grammar files
make build
make test-grammar

# Run the bootstrap comparison
make test-bootstrap
```

### In CI

The test runs automatically in GitHub Actions after all other tests. It's marked as **informational only** (`continue-on-error: true`), so differences won't fail the build.

## What the Test Does

The `compare-bootstrap.sh` script:

1. **Generates** files from `test-grammars/grammar.pg`:
   - `test-out/GrammarLexer.x`
   - `test-out/GrammarParser.y`
   - `test-out/GrammarQQ.hs`

2. **Compares** generated files with hand-written ones:
   - `Lexer.x` ↔ `test-out/GrammarLexer.x`
   - `Parser.y` ↔ `test-out/GrammarParser.y`

3. **Reports** differences (two-level check):
   - First: Exact match check
   - Second: Content match (using `diff -uwB` to ignore whitespace and blank lines)
   - ✓ Green: Files are identical (self-hosting achieved!)
   - ℹ Blue: Only whitespace differences (content is identical)
   - ⚠ Yellow: Content differences found (expected during development)
   - Shows line counts and diff statistics

## Interpreting Results

### If files are identical (✓)
RTK is fully self-hosting! The generated code matches hand-written code exactly.

### If only whitespace differs (ℹ)
The content is functionally identical! Only whitespace, indentation, or blank lines differ. This is excellent progress toward self-hosting.

### If files differ (⚠)
This is **expected** and shows what needs to be addressed:

- **Formatting differences**: Auto-generated code may format differently
- **Comments**: Hand-written files may have more documentation
- **Optimizations**: Hand-written code may include manual optimizations
- **Features**: Grammar may not yet capture all hand-written features

### Diff Options Used

The comparison uses `diff -uwB` for content checking:
- `-u`: Unified diff format (shows context)
- `-w`: Ignore all whitespace differences (spaces, tabs)
- `-B`: Ignore blank line changes

This focuses the comparison on actual semantic differences rather than formatting.

## Path to Self-Hosting

To achieve full self-hosting:

1. **Verify grammar completeness**: Ensure `test-grammars/grammar.pg` captures all RTK features
2. **Test equivalence**: Verify generated parsers handle all test cases
3. **Update imports**: Switch `main.hs` to use generated parsers
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

## Files

- `compare-bootstrap.sh` - Comparison script
- `test-grammars/grammar.pg` - RTK's grammar definition
- `Lexer.x` - Hand-written lexer (current)
- `Parser.y` - Hand-written parser (current)
- `.github/workflows/ci.yml` - CI configuration (runs test automatically)
- `makefile` - Build system (includes `test-bootstrap` target)

## Further Reading

- [Bootstrapping Compilers](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
- Self-hosting examples: GCC, Rust compiler, TypeScript
