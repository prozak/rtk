#!/bin/bash

# Profile RTK to identify performance bottlenecks

set -e

echo "RTK Profiling Script"
echo "===================="
echo ""

if ! command -v cabal &> /dev/null; then
    echo "Error: cabal not found"
    exit 1
fi

# Check if profiling libraries are available
echo "1. Building RTK with profiling support..."
cabal clean
cabal configure --enable-profiling --enable-library-profiling
cabal build --ghc-options="-fprof-auto -rtsopts"

echo ""
echo "2. Running RTK with profiling on java.pg..."
RTK_BIN=$(find dist-newstyle -name rtk -executable | head -n 1)

if [ -z "$RTK_BIN" ]; then
    echo "Error: RTK binary not found"
    exit 1
fi

mkdir -p profiling-results

# Run with time and heap profiling
time $RTK_BIN test-grammars/java.pg profiling-results/java +RTS -p -s -h -RTS

echo ""
echo "3. Analyzing results..."

if [ -f rtk.prof ]; then
    echo ""
    echo "Top 20 cost centres by time:"
    echo "----------------------------"
    grep -A 50 "COST CENTRE" rtk.prof | head -70

    echo ""
    echo "Full profile saved to: rtk.prof"
fi

if [ -f rtk.hp ]; then
    echo ""
    echo "Heap profile generated: rtk.hp"
    echo "Convert to PostScript with: hp2ps -c rtk.hp"
fi

echo ""
echo "Profiling complete! Results in profiling-results/"
