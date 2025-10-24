#!/bin/bash

# Compare original vs optimized build times

echo "Comparing original vs optimized build performance"
echo "================================================"
echo ""

# Clean everything
make clean > /dev/null 2>&1 || true

echo "Testing ORIGINAL makefile..."
echo "----------------------------"
time make test-java 2>&1 | grep -E "(real|user|sys)" || true

# Clean again
make clean > /dev/null 2>&1 || true

echo ""
echo "Testing OPTIMIZED makefile..."
echo "-----------------------------"
time make -f makefile.optimized test-java 2>&1 | grep -E "(real|user|sys)" || true

echo ""
echo "Testing INCREMENTAL build (no changes)..."
echo "-----------------------------------------"
time make -f makefile.optimized test-java 2>&1 | grep -E "(real|user|sys)" || true
