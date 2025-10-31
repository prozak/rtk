# RTK - Rewrite Toolkit

RTK is a Haskell-based tool that generates rewrite facilities for given grammars. It uses Alex for lexical analysis and Happy for parser generation.

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

### Option 2: Local Development

If you have Haskell installed locally:

```bash
# Build the project
make build

# Run tests
make test

# Run all Java tests
make test-all-java
```

See [Claude.MD](Claude.MD) for complete local development documentation.

## Prerequisites

### Docker Setup
- [Docker](https://docs.docker.com/get-docker/)
- [Docker Compose](https://docs.docker.com/compose/install/)

### Local Setup
- GHC 9.6.4+
- Cabal 3.10.2.0+
- Alex and Happy (auto-installed by Cabal)

## Usage

Generate a parser and lexer from a grammar file:

```bash
# Using Docker
docker-compose run --rm rtk-dev cabal exec rtk -- test-grammars/java.pg test-out

# Using local installation
cabal exec rtk -- test-grammars/java.pg test-out
```

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

GPL-3.0-or-later

## Author

Nickolay Lysenko (nickolay.lysenko@gmail.com)
