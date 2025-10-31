# Docker Setup Testing Guide

This document provides step-by-step instructions for testing the Docker development environment.

## Environment Limitations

**Note**: The Claude Code environment runs inside a container without Docker-in-Docker support, so Docker testing must be performed on your local machine with Docker installed.

## Prerequisites

Before testing, ensure you have:

1. **Docker Desktop** (macOS/Windows) or **Docker Engine** (Linux) installed
2. **Docker Compose** (usually included with Docker Desktop)

Installation guides:
- macOS/Windows: https://docs.docker.com/desktop/
- Linux: https://docs.docker.com/engine/install/

## Quick Test

Run the automated test script:

```bash
./test-docker-setup.sh
```

This will:
- ✓ Check Docker and Docker Compose are installed
- ✓ Build the development image
- ✓ Test all tools (GHC, Cabal, Alex, Happy)
- ✓ Build the RTK project
- ✓ Run the test suite

Expected time: **5-10 minutes** on first run, ~30 seconds on subsequent runs.

## Manual Testing Steps

If you prefer to test manually or the script fails:

### Step 1: Verify Docker Installation

```bash
docker --version
docker-compose --version  # or: docker compose version
```

Expected output:
```
Docker version 20.x.x or higher
Docker Compose version v2.x.x or higher
```

### Step 2: Build the Development Image

```bash
docker-compose build rtk-dev
```

Expected output:
- Downloads `haskell:9.6.4` base image (~500MB)
- Installs system dependencies
- Installs Alex and Happy
- Downloads Haskell dependencies
- Completes without errors

**First build**: 5-10 minutes
**Subsequent builds**: ~30 seconds (cached)

### Step 3: Verify Haskell Toolchain

```bash
# Check GHC
docker-compose run --rm rtk-dev ghc --version

# Expected: The Glorious Glasgow Haskell Compilation System, version 9.6.4

# Check Cabal
docker-compose run --rm rtk-dev cabal --version

# Expected: cabal-install version 3.x.x.x

# Check Alex
docker-compose run --rm rtk-dev alex --version

# Expected: Alex version 3.x.x

# Check Happy
docker-compose run --rm rtk-dev happy --version

# Expected: Happy Version 1.x.x
```

### Step 4: Test Project Build

```bash
docker-compose run --rm rtk-dev cabal build
```

Expected output:
- Builds all Haskell modules
- May show some warnings (this is normal)
- Completes with "Build succeeded"

### Step 5: Run Unit Tests

```bash
# Basic unit tests
docker-compose run --rm test

# Should run:
# - StrQuote tests
# - EmptyGrammar tests
```

### Step 6: Run Grammar Tests

```bash
# Test the grammar parser
docker-compose run --rm rtk-dev make test-grammar

# Test Java grammar
docker-compose run --rm rtk-dev make test-java

# Test Java quasi-quotation
docker-compose run --rm rtk-dev make test-java-qq
```

### Step 7: Run All Java Tests

```bash
docker-compose run --rm test-java
```

This runs the comprehensive Java test suite (~25 tests).

### Step 8: Interactive Development

```bash
# Start an interactive shell
docker-compose run --rm rtk-dev

# Inside the container, try:
cabal build
make test
cabal exec rtk -- test-grammars/java.pg test-out
ls test-out/  # Should show generated files
exit
```

## Validation Checklist

Use this checklist to confirm everything works:

- [ ] Docker and Docker Compose are installed
- [ ] `docker-compose build rtk-dev` completes without errors
- [ ] GHC 9.6.4 is available in the container
- [ ] Cabal is available in the container
- [ ] Alex and Happy are available
- [ ] `cabal build` succeeds
- [ ] Basic unit tests run (even if some fail)
- [ ] Grammar test generates output files
- [ ] Can run interactive shell
- [ ] Source code changes are visible in container
- [ ] Build artifacts persist between runs

## Common Issues and Solutions

### Issue: "docker: command not found"

**Solution**: Install Docker Desktop or Docker Engine for your platform.

### Issue: "permission denied while trying to connect to the Docker daemon"

**Solution** (Linux):
```bash
sudo usermod -aG docker $USER
# Log out and back in
```

### Issue: Build fails with "no space left on device"

**Solution**: Clean up Docker:
```bash
docker system prune -a
docker volume prune
```

### Issue: Build is very slow

**Cause**: First build downloads and compiles all dependencies.

**Solution**: Be patient. Subsequent builds will be much faster thanks to layer caching.

### Issue: Changes to source files not reflected in container

**Cause**: Volume mount issue.

**Solution**:
```bash
docker-compose down
docker-compose run --rm rtk-dev
```

### Issue: "cannot find cabal" in container

**Cause**: PATH issue.

**Solution**: Rebuild the image:
```bash
docker-compose build --no-cache rtk-dev
```

## Performance Expectations

| Operation | First Time | Subsequent |
|-----------|------------|------------|
| Build image | 5-10 min | 30 sec |
| Build project | 3-5 min | 10-30 sec |
| Run tests | 1-2 min | 1-2 min |
| Start shell | 2-3 sec | 2-3 sec |

## Comparing with Local Development

| Aspect | Local | Docker |
|--------|-------|--------|
| Setup time | 30-60 min | 5-10 min |
| Consistency | Varies by system | Identical |
| Isolation | System-wide | Container-only |
| CI parity | May differ | Exact match |

## Next Steps After Successful Testing

Once you've verified the Docker setup works:

1. **For development**: Use `docker-compose run --rm rtk-dev` as your primary environment
2. **For CI**: We can create a production Dockerfile and publish to GHCR
3. **For team**: Share the Docker setup for consistent environments

## Getting Help

If you encounter issues:

1. Check this guide's troubleshooting section
2. Review DOCKER.md for detailed usage
3. Check Docker logs: `docker-compose logs`
4. Try rebuilding from scratch: `docker-compose build --no-cache rtk-dev`

## Reporting Results

When reporting test results, include:

```bash
# System info
docker --version
docker-compose --version
uname -a

# Build output
docker-compose build rtk-dev 2>&1 | tee build.log

# Test output
./test-docker-setup.sh 2>&1 | tee test.log
```
