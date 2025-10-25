# RTK Project - Build and Development Guide

## Project Overview

RTK (Rewrite Toolkit) is a Haskell project that generates rewrite facilities for given grammars. It uses Alex for lexical analysis and Happy for parser generation.

## Prerequisites

The project requires:
- GHC (Glasgow Haskell Compiler) - version 9.4.7 or compatible
- Cabal (Haskell package manager) - version 3.8.1.0 or compatible
- Alex and Happy are automatically installed as build tools via Cabal

## Initial Setup

### 1. Install Haskell Toolchain (if not already installed)

On Ubuntu/Debian:
```bash
apt-get update
apt-get install -y ghc cabal-install
```

### 2. Update Cabal Package Index

```bash
cabal update
```

This downloads the latest package list from Hackage and should be run once after installation.

## Building the Project

### Quick Build

```bash
make build
```

Or directly with Cabal:
```bash
cabal build
```

### Build Process Details

The build process:
1. Automatically downloads and installs all Haskell dependencies (on first build)
2. Installs Alex and Happy as build tools (specified in rtk.cabal)
3. Compiles the lexer (Lexer.x) using Alex
4. Compiles the parser (Parser.y) using Happy
5. Compiles all Haskell source modules
6. Links the final executable

**Expected warnings**: The build produces several compiler warnings (unused imports, unused matches, missing signatures) which can be safely ignored. These are not errors.

### Build Output

The executable is created at:
```
dist-newstyle/build/x86_64-linux/ghc-9.4.7/rtk-0.10/x/rtk/build/rtk/rtk
```

(The exact path may vary based on your platform and GHC version)

## Running RTK

To run the compiled executable:

```bash
cabal exec rtk -- <pg-file> <output-directory>
```

Usage:
- `<pg-file>`: Path to a grammar file (.pg extension)
- `<output-directory>`: Directory where generated files will be written

Example:
```bash
cabal exec rtk -- test-grammars/java.pg test-out
```

## Testing

### Run All Tests

```bash
make test
```

### Run Specific Grammar Tests

Available test targets:
- `make test-grammar` - Test the grammar parser itself
- `make test-java` - Test Java grammar
- `make test-java-simple` - Test simplified Java grammar
- `make test-haskell` - Test Haskell grammar
- `make test-sandbox` - Test sandbox grammar
- `make test-t1` - Test t1 grammar

Example:
```bash
make test-java
```

## Cleaning Build Artifacts

```bash
make clean
```

This removes:
- All build artifacts in `dist-newstyle/`
- Test output directory `test-out/`

## Project Structure

```
rtk/
├── rtk.cabal           # Cabal build configuration
├── makefile            # Make build targets
├── main.hs             # Main executable entry point
├── Lexer.x             # Alex lexer specification
├── Parser.y            # Happy parser specification
├── Grammar.hs          # Grammar data structures
├── Normalize.hs        # Grammar normalization
├── GenAST.hs           # AST generation
├── GenX.hs             # Lexer code generation
├── GenY.hs             # Parser code generation
├── GenQ.hs             # Query code generation
└── test-grammars/      # Test grammar files
```

## Dependencies

Key Haskell packages (automatically installed by Cabal):
- base (core Haskell)
- array
- containers
- mtl
- lens
- syb
- template-haskell
- haskell-src-exts
- haskell-src-meta
- MissingH
- pretty-show
- HUnit (for testing)

## Troubleshooting

### "cabal: command not found"

Install the Haskell toolchain:
```bash
apt-get install -y ghc cabal-install
```

### "The program 'alex' is required but it could not be found"

This should be automatically resolved by Cabal as Alex is specified in Build-Tool-Depends. If the issue persists, manually install:
```bash
cabal install alex happy
```

### Build fails with dependency errors

Update Cabal and retry:
```bash
cabal update
cabal clean
cabal build
```

### Fresh build taking too long

First-time builds download and compile all dependencies, which can take 5-10 minutes. Subsequent builds are much faster.

## Quick Reference

| Task | Command |
|------|---------|
| Build project | `make build` or `cabal build` |
| Run executable | `cabal exec rtk -- <args>` |
| Run tests | `make test` |
| Clean build | `make clean` |
| Update packages | `cabal update` |
| Build specific grammar test | `make test-<grammar-name>` |

## Additional Notes

- The project uses Cabal's new-build system (nix-style builds)
- Build artifacts are isolated in `dist-newstyle/`
- Each grammar test generates .x (lexer) and .y (parser) files in `test-out/`
- The Makefile provides platform-specific handling for Windows and Unix-like systems
