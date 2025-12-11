# RTK - Rewrite ToolKit

RTK generates parser and rewrite facilities from grammar specifications. It produces Alex lexer and Happy parser files, with support for quasi-quotation to embed parsed syntax directly in Haskell code.

## Features

- **Grammar Specifications**: Define languages using `.pg` grammar files
- **Lexer Generation**: Generates Alex (`.x`) lexer specifications
- **Parser Generation**: Generates Happy (`.y`) parser specifications
- **Quasi-Quotation**: Embed parsed syntax in Haskell via Template Haskell
- **Self-Hosting**: RTK can parse and generate parsers for its own grammar format

## Quick Start

### Option 1: Docker (Recommended for Quick Testing)

The easiest way to get started without installing the Haskell toolchain:

```bash
# Build the development image
docker-compose build rtk-dev

# Run tests
docker-compose run --rm test

# Interactive shell
docker-compose run --rm rtk-dev
```

See [DOCKER.md](DOCKER.md) for complete Docker documentation.

### Option 2: Install from Hackage

```bash
cabal update
cabal install rtk
```

### Option 3: Build from Source

If you have Haskell installed locally:

```bash
# Build the project
cabal build

# Run tests
make test
```

See [Claude.MD](Claude.MD) for complete local development documentation.

## Prerequisites

### Docker Setup
- [Docker](https://docs.docker.com/get-docker/)
- [Docker Compose](https://docs.docker.com/compose/install/)

### Local Setup
- GHC >= 9.4
- Cabal >= 3.8
- Alex
- Happy

## Usage

Generate lexer and parser from a grammar file:

```bash
rtk <grammar-file>.pg <output-directory>
```

This creates:
- `<Grammar>Lexer.x` - Alex lexer specification
- `<Grammar>Parser.y` - Happy parser specification
- `<Grammar>QQ.hs` - Quasi-quoter module

Then compile with Alex and Happy:

```bash
alex <Grammar>Lexer.x -o <Grammar>Lexer.hs
happy <Grammar>Parser.y -o <Grammar>Parser.hs
```

### Using Docker

```bash
# Using Docker
docker-compose run --rm rtk-dev cabal exec rtk -- test-grammars/java.pg test-out

# Using local installation
cabal exec rtk -- test-grammars/java.pg test-out
```

## Grammar Format

Grammar files use a simple specification format:

```
grammar MyLang;

// Token definitions
INT = [0-9]+;
PLUS = "+";

// Grammar rules
Expr : Expr PLUS Term { Add $1 $3 }
     | Term           { $1 }
     ;

Term : INT { Lit (read $1) }
     ;
```

## Example Grammars

The `test-grammars/` directory contains example grammars:
- `java.pg` - Java language grammar
- `grammar.pg` - Grammar for the grammar language itself (bootstrap)
- `haskell.pg` - Haskell subset grammar

## Testing

```bash
# Basic unit tests
make test

# Grammar parser test
make test-grammar

# All Java tests
make test-all-java

# Java quasi-quotation tests
make test-java-qq
```

With Docker:

```bash
docker-compose run --rm test              # Basic tests
docker-compose run --rm test-java         # All Java tests
docker-compose run --rm rtk-dev make test-java-qq
```

## Project Structure

```
rtk/
├── *.hs                 # Haskell source files
├── Lexer.x              # Alex lexer specification
├── Parser.y             # Happy parser specification
├── rtk.cabal            # Cabal package configuration
├── makefile             # Build and test targets
├── test-grammars/       # Test grammar files
├── Dockerfile.dev       # Development Docker image
└── docker-compose.yml   # Docker Compose configuration
```

## Documentation

- **[DOCKER.md](DOCKER.md)** - Docker development environment setup and usage
- **[TESTING.md](TESTING.md)** - Docker testing guide
- **[Claude.MD](Claude.MD)** - Detailed local development guide
- **[CI Workflow](.github/workflows/ci.yml)** - GitHub Actions configuration

## Contributing

When making changes:

1. Run tests locally: `make test` or `docker-compose run --rm test`
2. Ensure all Java tests pass: `make test-all-java`
3. Commit and push to your branch
4. CI will run all tests automatically

## CI/CD

The project uses GitHub Actions for continuous integration. See [.github/workflows/ci.yml](.github/workflows/ci.yml) for details.

Current CI setup:
- Runs on: `ubuntu-latest`
- GHC version: `9.6.4`
- Tests: Basic unit tests, grammar tests, Java test suite, Java QQ tests

## License

MIT License - see [LICENSE](LICENSE) for details.

Generated code (lexers, parsers, quasi-quoters) produced by RTK is exempt from this license and may be used without restriction.

## Author

Nickolay Lysenko (nickolay.lysenko@gmail.com)
