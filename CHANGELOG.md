# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- Hackage packaging hygiene: PVP version bounds on all dependencies and on
  the `alex`/`happy` build tools, `Tested-With` now lists GHC 9.4.7 and
  9.6.4 (the versions actually exercised locally and in CI), and the
  packages required by RTK-generated code are documented in the README and
  the cabal description
- Source positions on tokens: the lexer now returns `PosToken` values
  (token plus line/column), both in the hand-written lexer and in all
  generated lexers
- Parse errors report line, column and a human-readable description of the
  unexpected token (e.g. `Parse error at line 2, column 1: unexpected
  identifier 'Foo'`); generated parsers report positions too instead of
  dumping the remaining token list
- Errors at end of input carry the position where the input ended, in both
  the hand-written and generated parsers
- Grammar normalization errors name the offending rule and its source
  position (e.g. `Grammar error in rule 'Foo' (at line 2, column 1): ...`)
- Lexer-generation errors name the lexical rule they occur in

### Removed
- Unimplemented CLI options that were advertised in `--help` but had no
  effect: `--debug-rule`, `--compare-stages`, `--memory-stats`,
  `--debug-output-dir`, `--debug-log`, `--interactive`, the placeholder
  `json`/`tree` debug formats, and the `--use-generated` stub that only
  printed an error

### Fixed
- A grammar whose first rule is lexical (or has a data-type annotation
  different from the rule name) no longer crashes with `fromJust: Nothing`;
  it reports that the first rule must be a syntax rule, or resolves the
  annotated type correctly
- Internal `fromJust` calls in code generation replaced with descriptive
  internal-error messages
- Invalid clauses in lexical-rule macros now abort generation with an error
  instead of writing the error text into the generated lexer
- User-facing errors no longer print a GHC call stack
- `--debug-stage` now exits with a success status after stopping at the
  requested stage instead of reporting failure via `error`
- `--profile-stages` timings now force each stage's result to normal form,
  so per-stage durations are no longer skewed by lazy evaluation

### Documentation
- Replaced the README "Grammar Format" example, which used Happy-style
  semantic actions that RTK cannot parse, with a verified `.pg` example
- Removed the stale `Claude.MD` (a case-colliding near-duplicate of
  `CLAUDE.md`; its Quick Reference table was folded into `CLAUDE.md`) and the
  stray root `test-simple-return.java` duplicate

## [0.10] - 2025-12-03

### Added
- MIT license with generated code exemption
- Full Java grammar support with comprehensive parsing tests
- Quasi-quotation support for embedding parsed syntax in Haskell
- Debug options for grammar development and troubleshooting
- Bootstrap self-hosting capability (RTK can parse its own grammar format)

### Fixed
- Alex escape sequence generation in GenX.hs
- Java grammar lexer patterns for complete test coverage

## [0.9] - Initial Development

### Added
- Core grammar specification format (.pg files)
- Alex lexer generation (GenX.hs)
- Happy parser generation (GenY.hs)
- AST generation (GenAST.hs)
- Quasi-quotation generation (GenQ.hs)
- Grammar normalization and transformation
