# Docker Development Environment

This document describes how to use Docker for local development and testing of RTK.

## Prerequisites

- [Docker](https://docs.docker.com/get-docker/) installed
- [Docker Compose](https://docs.docker.com/compose/install/) installed (usually comes with Docker Desktop)

## Quick Start

### Build the Development Image

```bash
docker-compose build rtk-dev
```

This will:
- Download the Haskell 9.6.4 base image
- Install build tools (Alex, Happy)
- Pre-download project dependencies
- Cache everything for faster subsequent builds

### Run Tests

```bash
# Run basic unit tests
docker-compose run --rm test

# Run all Java tests
docker-compose run --rm test-java

# Run a specific test
docker-compose run --rm rtk-dev make test-grammar
```

### Build the Project

```bash
docker-compose run --rm build
```

### Interactive Development

Start an interactive shell in the container:

```bash
docker-compose run --rm rtk-dev
```

Inside the container, you can:

```bash
# Build the project
cabal build

# Run specific tests
make test
make test-grammar
make test-all-java

# Generate parsers for a specific grammar
cabal exec rtk -- test-grammars/java.pg test-out

# Clean build artifacts
make clean
```

### One-off Commands

Run any command in the container:

```bash
docker-compose run --rm rtk-dev cabal build
docker-compose run --rm rtk-dev make test-java-qq
docker-compose run --rm rtk-dev ghc --version
```

## Volume Mounts

The `docker-compose.yml` configuration uses volume mounts to:

1. **Source code**: Your local directory is mounted at `/workspace` in the container
   - Changes you make locally are immediately visible in the container
   - Changes made in the container are immediately visible locally

2. **Cabal cache**: Persistent volume for downloaded packages
   - Speeds up subsequent builds
   - Survives container restarts

3. **Build artifacts**: Persistent volume for compiled code
   - Faster rebuilds
   - Survives container restarts

## Cleaning Up

Remove all volumes (this will require re-downloading dependencies):

```bash
docker-compose down -v
```

Remove the image:

```bash
docker rmi rtk:dev
```

## Benefits of Docker Development

1. **Consistency**: Same environment as CI
2. **Isolation**: No interference with system Haskell installation
3. **Easy setup**: No need to install GHC, Cabal, Alex, Happy manually
4. **Clean slate**: Easy to start fresh with `docker-compose down -v`
5. **Portability**: Works on Linux, macOS, and Windows

## Troubleshooting

### Build is slow

First build will take 5-10 minutes to download and compile dependencies. Subsequent builds are much faster thanks to Docker layer caching and persistent volumes.

### Permission errors

If you get permission errors on test-out or build artifacts:

```bash
docker-compose run --rm rtk-dev chown -R $(id -u):$(id -g) /workspace
```

### Out of disk space

Clean up Docker resources:

```bash
# Remove unused containers, networks, images
docker system prune

# Remove unused volumes
docker volume prune
```

### Want to rebuild from scratch

```bash
docker-compose build --no-cache rtk-dev
```

## Comparison: Local vs Docker

| Task | Local | Docker |
|------|-------|--------|
| Initial setup | Install GHC, Cabal, Alex, Happy | `docker-compose build` |
| Run tests | `make test` | `docker-compose run --rm test` |
| Interactive dev | Direct | `docker-compose run --rm rtk-dev` |
| CI consistency | May differ | Identical environment |
| Isolation | Uses system tools | Fully isolated |

## Next Steps

Once you verify the Docker setup works locally, we can:
1. Create an optimized production Dockerfile for CI
2. Update GitHub Actions to use the Docker image
3. Publish the image to GitHub Container Registry for faster CI runs
