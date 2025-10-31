#!/bin/bash

# Docker Setup Validation and Test Script
# This script tests the Docker development environment for RTK

set -e

echo "=========================================="
echo "RTK Docker Setup Test Script"
echo "=========================================="
echo ""

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Function to print status
print_status() {
    if [ $1 -eq 0 ]; then
        echo -e "${GREEN}✓${NC} $2"
    else
        echo -e "${RED}✗${NC} $2"
        exit 1
    fi
}

print_warning() {
    echo -e "${YELLOW}⚠${NC} $1"
}

# Check prerequisites
echo "Checking prerequisites..."
echo ""

if command -v docker &> /dev/null; then
    print_status 0 "Docker is installed"
    docker --version
else
    print_status 1 "Docker is NOT installed"
fi

if command -v docker-compose &> /dev/null; then
    print_status 0 "Docker Compose is installed"
    docker-compose --version
elif docker compose version &> /dev/null; then
    print_status 0 "Docker Compose (plugin) is installed"
    docker compose version
    COMPOSE_CMD="docker compose"
else
    print_status 1 "Docker Compose is NOT installed"
fi

# Set compose command
if [ -z "$COMPOSE_CMD" ]; then
    COMPOSE_CMD="docker-compose"
fi

echo ""
echo "=========================================="
echo "Building Docker Image"
echo "=========================================="
echo ""
echo "This will take 5-10 minutes on first run..."
echo ""

if $COMPOSE_CMD build rtk-dev; then
    print_status 0 "Docker image built successfully"
else
    print_status 1 "Docker image build failed"
fi

echo ""
echo "=========================================="
echo "Testing Basic Commands"
echo "=========================================="
echo ""

echo "Testing GHC version..."
if $COMPOSE_CMD run --rm rtk-dev ghc --version; then
    print_status 0 "GHC is available in container"
else
    print_status 1 "GHC test failed"
fi

echo ""
echo "Testing Cabal version..."
if $COMPOSE_CMD run --rm rtk-dev cabal --version; then
    print_status 0 "Cabal is available in container"
else
    print_status 1 "Cabal test failed"
fi

echo ""
echo "Testing Alex availability..."
if $COMPOSE_CMD run --rm rtk-dev alex --version; then
    print_status 0 "Alex is available in container"
else
    print_status 1 "Alex test failed"
fi

echo ""
echo "Testing Happy availability..."
if $COMPOSE_CMD run --rm rtk-dev happy --version; then
    print_status 0 "Happy is available in container"
else
    print_status 1 "Happy test failed"
fi

echo ""
echo "=========================================="
echo "Building RTK Project"
echo "=========================================="
echo ""

if $COMPOSE_CMD run --rm rtk-dev cabal build; then
    print_status 0 "RTK project built successfully"
else
    print_status 1 "RTK build failed"
fi

echo ""
echo "=========================================="
echo "Running Tests"
echo "=========================================="
echo ""

echo "Running basic unit tests..."
if $COMPOSE_CMD run --rm test; then
    print_status 0 "Basic tests passed"
else
    print_warning "Basic tests had failures (this may be expected)"
fi

echo ""
echo "Running grammar test..."
if $COMPOSE_CMD run --rm rtk-dev make test-grammar; then
    print_status 0 "Grammar test passed"
else
    print_warning "Grammar test had failures"
fi

echo ""
echo "=========================================="
echo "All Docker Setup Tests Complete!"
echo "=========================================="
echo ""
echo "You can now use:"
echo "  $COMPOSE_CMD run --rm rtk-dev           # Interactive shell"
echo "  $COMPOSE_CMD run --rm test              # Run tests"
echo "  $COMPOSE_CMD run --rm test-java         # Run Java tests"
echo "  $COMPOSE_CMD run --rm rtk-dev make <target>  # Run any make target"
echo ""
echo "See DOCKER.md for complete documentation."
echo ""
