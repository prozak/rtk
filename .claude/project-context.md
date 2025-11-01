# RTK Project Context

## Project Overview
RTK (Rewrite ToolKit) is a tool for generating parser and rewrite facilities from grammar specifications.

**Language**: Haskell
**Build System**: Cabal + Make
**Version**: 0.10

## What RTK Does

1. **Input**: Grammar specification files (.pg format)
2. **Output**: Generates Alex lexer (.x) and Happy parser (.y) files
3. **Feature**: Supports quasi-quotation for embedding parsed syntax in Haskell

## Key Components

### Core Modules
- `Grammar.hs` - Grammar data structures and AST definitions
- `Lexer.x` - Hand-written lexer for grammar files (Alex spec)
- `Parser.y` - Hand-written parser for grammar files (Happy spec)
- `Normalize.hs` - Grammar normalization and transformation
- `GenAST.hs`, `GenQ.hs`, `GenX.hs`, `GenY.hs` - Code generators

### Build Tools
- `rtk.cabal` - Cabal package configuration
- `makefile` - Test orchestration and build automation

### Test Grammars
- `test-grammars/java.pg` - Java language grammar
- `test-grammars/grammar.pg` - Grammar for the grammar language itself
- Various test files in `test-grammars/java/`

## Bootstrap Self-Hosting

RTK is **self-hosting** - it can parse and generate parsers for its own grammar language:
1. Hand-written `Lexer.x` and `Parser.y` parse grammar files
2. `test-grammars/grammar.pg` describes the grammar language itself
3. RTK can generate lexer/parser from `grammar.pg`
4. `compare-bootstrap.sh` compares hand-written vs generated versions

## Typical Workflow

```bash
# Generate lexer and parser from a grammar
rtk <grammar-file>.pg <output-directory>

# This creates:
# - <output-directory>/<Grammar>Lexer.x
# - <output-directory>/<Grammar>Parser.y
# - <output-directory>/<Grammar>QQ.hs (quasi-quoter)

# Then use Alex and Happy to generate Haskell code:
alex <Grammar>Lexer.x -o <Grammar>Lexer.hs
happy <Grammar>Parser.y -o <Grammar>Parser.hs
```

## Dependencies

### External Tools Required
- Alex (lexer generator)
- Happy (parser generator)

### Key Haskell Packages
- `base` - Standard library
- `syb` - Scrap Your Boilerplate (generic programming)
- `template-haskell` - Template Haskell metaprogramming
- `haskell-src-exts` - Haskell source manipulation
- `haskell-src-meta` - Converting between TH and HSE
- `lens` - Lens-based accessors
- `HUnit` - Unit testing
- `containers`, `mtl`, `pretty`, `pretty-show`, `MissingH`

## Environment Requirements

**Operating System**: Linux (tested on Ubuntu)
**Architecture**: x86_64
**Runtime**: Root user environment

**Critical Environment Variables**:
- `PATH` must include `~/.cabal/bin` for alex/happy
- `LANG=C.UTF-8` and `LC_ALL=C.UTF-8` for proper Unicode handling in tests

## Testing

The project has comprehensive tests:
- Unit tests for core functionality
- Integration tests for Java grammar parsing
- Quasi-quotation tests for embedding Java syntax
- Bootstrap self-hosting comparison test

All tests pass successfully as of 2025-11-01.

## Development Notes

**File Encoding**: UTF-8 required for proper test output
**Build Warnings**: Some warnings about unused imports/matches are expected
**Parser Conflicts**: Java grammar has known shift/reduce and reduce/reduce conflicts (this is normal for complex grammars)

## Documentation

- `BOOTSTRAP.md` - Bootstrap self-hosting documentation
- `Claude.MD` - Development context (possibly for AI assistants)
- `docs/java-quasi-quotation-tests.md` - Java QQ test documentation
