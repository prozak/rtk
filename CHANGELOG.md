# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

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
