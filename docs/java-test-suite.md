# Java Test Suite

Testing the RTK Java grammar against real-world Java 8 codebases.

## Quick Start

```bash
# 1. Clone and build RTK
git clone <repo-url>
cd rtk
make build

# 2. Initialize test suites (git submodules)
git submodule update --init --recursive

# 3. Run Apache Commons Lang test suite
make test-suite-commons-lang

# 4. Analyze failures
make analyze-failures
```

## Test Suites

### Apache Commons Lang
- **Location**: `test-suites/commons-lang`
- **Files**: 259 main source files, 267 test files
- **Version**: Java 8 compatible (3.19.0+)
- **Description**: High-quality utility library testing core Java features

## Usage

### Run Tests

```bash
# Full parsing tests (informational only)
make test-suite-commons-lang           # Main sources (259 files)
make test-suite-commons-lang-all       # All sources (526 files)

# Lexical parsing tests (required - fails on errors)
make test-lex-commons-lang             # Main sources (259 files)
make test-lex-commons-lang-all         # All sources (526 files)

# Analyze failures
make analyze-failures DIR=test-results/commons-lang-main
```

### Test Scripts

- **test-java-suite.sh**: Automated test runner with progress bar
  - Supports `--lex-only` flag for lexical parsing only
  - Usage: `./test-java-suite.sh [--lex-only] <directory> [output-dir]`
- **analyze-failures.sh**: Categorizes errors and generates reports

### Adding New Test Suites

```bash
# 1. Add as git submodule
git submodule add <url> test-suites/<name>

# 2. Add Makefile target
test-suite-<name>: build
	./test-java-suite.sh test-suites/<name>/src/main/java test-results/<name>

# 3. Update .PHONY
.PHONY: test-suite-<name>
```

## Test Results Format

Results are saved to `test-results/<suite-name>/`:
```
test-results/commons-lang-main/
├── report.txt              # Summary statistics
├── succeeded/              # Successful parses
│   ├── list.txt
│   └── [files].java
└── failed/                 # Failed parses
    ├── list.txt
    └── [files].java.err
```

## Interpreting Results

The parser currently supports Java 8 syntax **except**:
- Lambda expressions (`() -> {}`)
- Method references (`Class::method`)
- Default methods in interfaces
- Type annotations

Many test failures are expected for code using these features.

## CI Integration

### Lexical Parsing Tests (Required)

Lexical parsing tests run as **required** tests in CI and will fail the build if errors occur:
```yaml
run_test "Apache Commons Lexical Tests" "make test-lex-commons-lang-all"
```

This ensures that the Java lexer can tokenize all Apache Commons Java files without errors.

### Full Parsing Tests (Informational)

Full parsing tests run as informational steps (continue-on-error) to track progress:
```yaml
- name: Apache Commons Lang test suite (informational)
  continue-on-error: true
  run: make test-suite-commons-lang
```

This tracks parser progress without blocking CI, as many files use unsupported Java 8 features (lambdas, method references, etc.).
