#!/bin/bash

# Java Grammar Build Speed Benchmark
# Measures build times for different phases of the Java grammar compilation

set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Configuration
GRAMMAR_FILE="test-grammars/java.pg"
OUTPUT_DIR="test-out"
BIN_PATH="dist-newstyle/build/x86_64-linux/ghc-*/rtk-0.10/x/rtk/build/rtk/rtk"
ITERATIONS=5

echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Java Grammar Build Speed Benchmark${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Ensure clean state
clean_build() {
    echo -e "${YELLOW}Cleaning build artifacts...${NC}"
    rm -rf "$OUTPUT_DIR"
    mkdir -p "$OUTPUT_DIR"
    make clean > /dev/null 2>&1 || true
}

# Time a command and return milliseconds
time_command() {
    local description="$1"
    shift

    local total_time=0
    local times=()

    echo -e "${GREEN}Benchmarking: ${description}${NC}"

    for i in $(seq 1 $ITERATIONS); do
        # Clean between iterations for accurate measurements
        if [ "$3" == "clean" ]; then
            rm -rf "$OUTPUT_DIR"
            mkdir -p "$OUTPUT_DIR"
        fi

        local start=$(date +%s%N)
        eval "$@" > /dev/null 2>&1
        local end=$(date +%s%N)

        local duration=$(( (end - start) / 1000000 )) # Convert to ms
        times+=($duration)
        total_time=$((total_time + duration))

        echo -e "  Iteration $i: ${duration}ms"
    done

    local avg_time=$((total_time / ITERATIONS))

    # Calculate standard deviation
    local sum_sq=0
    for time in "${times[@]}"; do
        local diff=$((time - avg_time))
        sum_sq=$((sum_sq + diff * diff))
    done
    local variance=$((sum_sq / ITERATIONS))
    local std_dev=$(echo "sqrt($variance)" | bc)

    echo -e "  ${BLUE}Average: ${avg_time}ms (±${std_dev}ms)${NC}"
    echo ""

    # Return average time via global variable
    LAST_TIME=$avg_time
}

# Find the rtk binary
find_rtk_binary() {
    local bin=$(find dist-newstyle -name rtk -type f -executable 2>/dev/null | head -n 1)
    if [ -z "$bin" ]; then
        echo -e "${RED}Error: rtk binary not found. Run 'cabal build' first.${NC}"
        exit 1
    fi
    echo "$bin"
}

# Ensure rtk is built
echo -e "${YELLOW}Ensuring rtk is built...${NC}"
if ! cabal build > /dev/null 2>&1; then
    echo -e "${RED}Error: Failed to build rtk${NC}"
    exit 1
fi

RTK_BIN=$(find_rtk_binary)
echo -e "${GREEN}Using rtk binary: $RTK_BIN${NC}"
echo ""

# Phase 1: RTK Grammar Generation
echo -e "${BLUE}=== Phase 1: RTK Grammar Generation ===${NC}"
time_command "RTK grammar generation (java.pg → .y/.x files)" \
    "$RTK_BIN $GRAMMAR_FILE -o $OUTPUT_DIR/Java" "clean"
RTK_TIME=$LAST_TIME

# Phase 2: Happy Parser Generation
echo -e "${BLUE}=== Phase 2: Happy Parser Generation ===${NC}"
time_command "Happy parser compilation (.y → .hs)" \
    "happy -o $OUTPUT_DIR/JavaParser.hs $OUTPUT_DIR/JavaParser.y"
HAPPY_TIME=$LAST_TIME

# Phase 3: Alex Lexer Generation
echo -e "${BLUE}=== Phase 3: Alex Lexer Generation ===${NC}"
time_command "Alex lexer compilation (.x → .hs)" \
    "alex -o $OUTPUT_DIR/JavaLexer.hs $OUTPUT_DIR/JavaLexer.x"
ALEX_TIME=$LAST_TIME

# Phase 4: GHC Compilation
echo -e "${BLUE}=== Phase 4: GHC Compilation ===${NC}"
cat > "$OUTPUT_DIR/JavaMain.hs" << 'EOF'
module Main where
import JavaParser
import JavaLexer
import Text.Show.Pretty (ppShow)

main :: IO ()
main = do
    putStrLn "Java parser compiled successfully"
EOF

time_command "GHC compilation (all .hs → executable)" \
    "cd $OUTPUT_DIR && ghc -O0 --make JavaMain.hs -o JavaTest"
GHC_TIME=$LAST_TIME

# Phase 5: Runtime Parsing Performance
echo -e "${BLUE}=== Phase 5: Runtime Parsing Performance ===${NC}"

# Test with different file sizes
for test_file in test-grammars/TestMinimal.java test-grammars/TestBasic.java test-grammars/Test.java; do
    if [ -f "$test_file" ]; then
        file_size=$(wc -c < "$test_file")
        file_lines=$(wc -l < "$test_file")

        echo -e "${GREEN}Testing with: $(basename $test_file) (${file_size} bytes, ${file_lines} lines)${NC}"

        # Create a simple test runner
        cat > "$OUTPUT_DIR/JavaTestRunner.hs" << EOF
module Main where
import JavaParser
import JavaLexer
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    let filename = head args
    contents <- readFile filename
    let tokens = runLexer contents
    let result = runParser tokens
    putStrLn "Parse successful"
EOF

        ghc -O0 --make "$OUTPUT_DIR/JavaTestRunner.hs" -i"$OUTPUT_DIR" -o "$OUTPUT_DIR/JavaTestRunner" > /dev/null 2>&1

        time_command "  Parsing $(basename $test_file)" \
            "$OUTPUT_DIR/JavaTestRunner $test_file"

        echo -e "  ${BLUE}Throughput: $((file_lines * 1000 / LAST_TIME)) lines/second${NC}"
        echo ""
    fi
done

# Summary
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Build Time Summary${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

TOTAL_TIME=$((RTK_TIME + HAPPY_TIME + ALEX_TIME + GHC_TIME))

printf "%-35s %10s %8s\n" "Phase" "Time (ms)" "Percent"
echo "-------------------------------------------------------"
printf "%-35s %10d %7.1f%%\n" "RTK Grammar Generation" $RTK_TIME $(echo "scale=1; $RTK_TIME * 100 / $TOTAL_TIME" | bc)
printf "%-35s %10d %7.1f%%\n" "Happy Parser Compilation" $HAPPY_TIME $(echo "scale=1; $HAPPY_TIME * 100 / $TOTAL_TIME" | bc)
printf "%-35s %10d %7.1f%%\n" "Alex Lexer Compilation" $ALEX_TIME $(echo "scale=1; $ALEX_TIME * 100 / $TOTAL_TIME" | bc)
printf "%-35s %10d %7.1f%%\n" "GHC Compilation" $GHC_TIME $(echo "scale=1; $GHC_TIME * 100 / $TOTAL_TIME" | bc)
echo "-------------------------------------------------------"
printf "%-35s %10d\n" "TOTAL BUILD TIME" $TOTAL_TIME
echo ""

# Analysis and recommendations
echo -e "${BLUE}========================================${NC}"
echo -e "${BLUE}Optimization Opportunities${NC}"
echo -e "${BLUE}========================================${NC}"
echo ""

# Grammar statistics
echo -e "${GREEN}Grammar Statistics:${NC}"
grep -v "^[[:space:]]*#" $GRAMMAR_FILE | grep -v "^[[:space:]]*$" | wc -l | xargs -I {} echo "  Non-comment lines: {}"
grep "::=" $GRAMMAR_FILE | wc -l | xargs -I {} echo "  Production rules: {}"
grep "|" $GRAMMAR_FILE | wc -l | xargs -I {} echo "  Alternative productions: {}"

echo ""
echo -e "${GREEN}Generated Parser Statistics:${NC}"
wc -l "$OUTPUT_DIR/JavaParser.y" | awk '{print "  Parser spec lines: " $1}'
wc -l "$OUTPUT_DIR/JavaLexer.x" | awk '{print "  Lexer spec lines: " $1}'
if [ -f "$OUTPUT_DIR/JavaParser.hs" ]; then
    wc -l "$OUTPUT_DIR/JavaParser.hs" | awk '{print "  Generated parser code: " $1}'
fi

echo ""
echo -e "${YELLOW}Optimization Recommendations:${NC}"
echo ""

# Provide targeted recommendations based on timing
if [ $RTK_TIME -gt $((TOTAL_TIME * 30 / 100)) ]; then
    echo -e "${YELLOW}1. RTK Grammar Generation (High Impact)${NC}"
    echo "   - Profile RTK with +RTS -p to identify hotspots"
    echo "   - Consider caching normalized grammar AST"
    echo "   - Optimize Normalize.hs and grammar transformations"
    echo ""
fi

if [ $HAPPY_TIME -gt $((TOTAL_TIME * 30 / 100)) ]; then
    echo -e "${YELLOW}2. Happy Parser Generation (High Impact)${NC}"
    echo "   - Reduce grammar conflicts (currently at 149)"
    echo "   - Simplify production rules where possible"
    echo "   - Consider using LALR(1) optimizations"
    echo ""
fi

if [ $GHC_TIME -gt $((TOTAL_TIME * 40 / 100)) ]; then
    echo -e "${YELLOW}3. GHC Compilation (High Impact)${NC}"
    echo "   - Use -O0 for development builds (already doing)"
    echo "   - Split large generated modules"
    echo "   - Consider incremental compilation"
    echo ""
fi

echo -e "${YELLOW}4. General Optimizations${NC}"
echo "   - Cache generated files when grammar hasn't changed"
echo "   - Use parallel compilation (ghc -j)"
echo "   - Implement incremental rebuilds in makefile"
echo "   - Consider grammar modularization"
echo ""

echo -e "${GREEN}Benchmark complete!${NC}"
