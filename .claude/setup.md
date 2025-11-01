# RTK Project - Haskell Environment Setup Guide

## Environment Status
This project requires a Haskell build environment. The setup has been verified and working as of 2025-11-01.

## Installation Restrictions
- **GHCup installer is BLOCKED**: Cannot access https://get-ghcup.haskell.org (403 error)
- **Sudo is NOT available**: Permission issues with /etc/sudo.conf
- **Running as root**: Can use apt-get directly without sudo

## Required Packages
- GHC (Glasgow Haskell Compiler): version 9.4.7
- Cabal (Build tool): version 3.8.1.0
- Happy (Parser generator): version 2.1.7
- Alex (Lexer generator): version 3.5.4.0

## Installation Steps

### 1. Install GHC and Cabal via apt
```bash
apt-get install -y ghc cabal-install
```

This installs:
- GHC 9.4.7
- Cabal 3.8.1.0
- Required dependencies (libgmp-dev, libbsd-dev, libmd-dev)

### 2. Update Cabal Package Index
```bash
cabal update
```

This creates ~/.cabal/config and downloads the Hackage package list.

### 3. Install Build Tools (Happy and Alex)
```bash
cabal install happy alex
```

This installs the tools to ~/.cabal/bin/

### 4. Update PATH Environment Variable
```bash
export PATH="/root/.cabal/bin:$PATH"
```

This makes happy and alex available for the build process.

### 5. Build the RTK Project
```bash
cabal build
```

This will:
- Download and build ~58 Haskell dependencies
- Build the RTK library and executable
- Place the executable in: dist-newstyle/build/x86_64-linux/ghc-9.4.7/rtk-0.10/x/rtk/build/rtk/rtk

## Environment Variables for Tests

When running tests, set UTF-8 locale to avoid encoding issues:
```bash
export LANG=C.UTF-8
export LC_ALL=C.UTF-8
export PATH="/root/.cabal/bin:$PATH"
```

## Running Tests

### Basic Unit Tests
```bash
make test
```

Runs:
- StrQuote_Test (string quotation handling)
- EmptyGrammar_Test (grammar parsing edge cases)

### Bootstrap Comparison Test
```bash
make test-bootstrap
```

Compares hand-written grammar parser with auto-generated version.

### Java Grammar Tests
```bash
make test-java-minimal        # Minimal Java file
make test-java-qq             # Java quasi-quotation tests
make test-all-java            # All Java tests
```

## Verification Commands

After setup, verify installation:
```bash
ghc --version        # Should show: 9.4.7
cabal --version      # Should show: 3.8.1.0
~/.cabal/bin/happy --version   # Should show: 2.1.7
~/.cabal/bin/alex --version    # Should show: 3.5.4.0
```

## Build Artifacts

- **Executable**: `dist-newstyle/build/x86_64-linux/ghc-9.4.7/rtk-0.10/x/rtk/build/rtk/rtk`
- **Test output**: `test-out/` directory
- **Cabal binaries**: `~/.cabal/bin/` (happy, alex)
- **Cabal packages**: `~/.cabal/store/`

## Common Issues

### Issue: Unicode character encoding errors in test output
**Solution**: Set UTF-8 locale before running tests:
```bash
export LANG=C.UTF-8
export LC_ALL=C.UTF-8
```

### Issue: alex/happy not found during build
**Solution**: Ensure PATH includes ~/.cabal/bin:
```bash
export PATH="/root/.cabal/bin:$PATH"
```

### Issue: Package update fails with mirror warnings
**Expected**: Mirror lookup may fail but package updates still work.
```
Warning: Caught exception during _mirrors lookup:res_query: does not exist
Warning: No mirrors found for http://hackage.haskell.org/
```
This is normal and can be ignored.

## Quick Setup Script

For fresh environment setup, run all steps at once:
```bash
# Install base packages
apt-get install -y ghc cabal-install

# Update cabal
cabal update

# Install build tools
cabal install happy alex

# Set environment
export PATH="/root/.cabal/bin:$PATH"
export LANG=C.UTF-8
export LC_ALL=C.UTF-8

# Build project
cabal build

# Run tests
make test
```

## Project Build System

The project uses both Cabal and Make:
- **Cabal**: For building Haskell code and managing dependencies
- **Make**: For test orchestration and grammar file generation

See `makefile` for available targets.
