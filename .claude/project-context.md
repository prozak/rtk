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

## Git Commit Practices

**CRITICAL - Keep Commits Minimal and Clean**:

When working on a PR, follow these strict guidelines:

1. **Minimize commit count**: Keep the number of commits as small as possible
2. **Only commit when necessary**: Don't create intermediate commits for work-in-progress changes
3. **Avoid anti-patterns**:
   - ❌ NEVER introduce code in one commit and remove it in a later commit of the same PR
   - ❌ NEVER make multiple commits for small, related changes
   - ❌ NEVER commit debugging code, temporary changes, or experimental code that will be removed later
4. **Use git commit --amend**: When making corrections or additions to recent work, amend the existing commit rather than creating a new one
5. **Think before committing**: Plan your changes completely before making the first commit
6. **Batch related changes**: Group all related changes into a single, well-crafted commit

**Good commit practices**:
- ✅ Complete the entire feature/fix before committing
- ✅ Review all changes thoroughly before creating the commit
- ✅ Write clear, descriptive commit messages that explain the "why"
- ✅ If you need to make corrections, use `git commit --amend` instead of new commits
- ✅ Only create multiple commits when working on truly separate, independent features

**Remember**: A clean git history with minimal, meaningful commits is far better than many small, incremental commits that show the messy development process.

## Debugging Guidelines

**CRITICAL - Bash Command Limitations in Claude Code**:

Commands like `tail`, `head`, `cat`, etc. exist but **FAIL when used in pipes after PATH exports**:

```bash
# ❌ FAILS - tail: command not found
export PATH="/root/.cabal/bin:$PATH" && cabal build | tail -50

# ❌ FAILS - same issue with semicolon
export PATH="/root/.cabal/bin:$PATH"; echo "test" | tail -1

# ✅ WORKS - but impractical for most debugging
cabal build | /usr/bin/tail -50
```

**Root Cause**: When `export PATH=...` is used with `&&` or `;` in the same command as a pipe, the piped commands (like `tail`, `head`) cannot be found, even though they exist at `/usr/bin/tail`, etc.

**SOLUTION - Always Use Claude Code Tools**:

Claude Code provides specialized tools (Read, Grep, Glob, Edit, Write) that are **NOT bash commands** - they are direct function calls to Claude Code's infrastructure, completely independent of the shell environment.

**Why Claude Code Tools Work**:
- `tail`, `head`, `cat` = Bash commands executed through `/bin/bash` → Subject to PATH issues
- `Read`, `Grep`, `Glob` = Claude Code tools invoked via API → No shell involved, no PATH dependency

**Read Tool Benefits**:
- **NEVER use `tail`, `head`, or `cat`** - they fail when PATH is modified
- **ALWAYS use the Read tool** - it's a Claude Code function, not a bash command
- Provides:
  - Automatic line numbering (cat -n format)
  - Efficient reading of specific line ranges with offset/limit parameters
  - Better handling of large files
  - Ability to read from any offset (for viewing end of file)
  - Works regardless of PATH, environment variables, or shell state

**Examples**:
- ❌ Will fail: `cabal build 2>&1 | tail -50`
- ❌ Will fail: `cat test-out/JavaParser.hs | head -100`
- ✅ Reliable: Use Read tool with file_path and optional offset/limit parameters
- ✅ For build output: Redirect to file first, then use Read tool

**Other debugging practices**:
- Use **Grep tool** for searching file contents (not `grep` or `rg` bash commands)
- Use **Glob tool** for finding files by pattern (not `find` or `ls` bash commands)
- Use **Edit tool** for modifying files (not `sed` or `awk` bash commands)
- Reserve bash commands for actual system operations (git, build tools, package managers, etc.)

## Documentation

- `BOOTSTRAP.md` - Bootstrap self-hosting documentation
- `Claude.MD` - Development context (possibly for AI assistants)
- `docs/java-quasi-quotation-tests.md` - Java QQ test documentation
