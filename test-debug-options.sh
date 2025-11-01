#!/bin/bash

# Comprehensive test script for all RTK debug options
# Tests each option to ensure it produces expected output
# Note: set -e is NOT used so all tests run even if some fail

GRAMMAR="test-grammars/grammar.pg"
OUT_DIR="test-out"
RTK_EXEC="cabal exec rtk --"

# Colors for output
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

passed=0
failed=0

# Function to run a test
test_option() {
    local test_name="$1"
    local option="$2"
    local expected_pattern="$3"

    echo -n "Testing $test_name... "

    # Run rtk with the option and capture output
    if output=$($RTK_EXEC "$GRAMMAR" "$OUT_DIR" $option 2>&1); then
        # Check if expected pattern is in output
        if echo "$output" | grep -q "$expected_pattern"; then
            echo -e "${GREEN}PASS${NC}"
            ((passed++))
            return 0
        else
            echo -e "${RED}FAIL${NC} - Expected pattern '$expected_pattern' not found in output"
            ((failed++))
            return 1
        fi
    else
        echo -e "${RED}FAIL${NC} - Command failed with exit code $?"
        ((failed++))
        return 1
    fi
}

# Function to test option with file output
test_option_file() {
    local test_name="$1"
    local option="$2"
    local check_file="$3"

    echo -n "Testing $test_name... "

    # Clean up old output file
    rm -f "$check_file"

    # Run rtk with the option
    if $RTK_EXEC "$GRAMMAR" "$OUT_DIR" $option > "$check_file" 2>&1; then
        # Check if file was created and has content
        if [ -f "$check_file" ] && [ -s "$check_file" ]; then
            echo -e "${GREEN}PASS${NC}"
            ((passed++))
            return 0
        else
            echo -e "${RED}FAIL${NC} - Output file empty or not created"
            ((failed++))
            return 1
        fi
    else
        echo -e "${RED}FAIL${NC} - Command failed"
        ((failed++))
        return 1
    fi
}

# Function to test option that is expected to exit with error
test_option_with_error() {
    local test_name="$1"
    local option="$2"
    local expected_pattern="$3"

    echo -n "Testing $test_name... "

    # Run rtk with the option and capture output (expecting failure)
    output=$($RTK_EXEC "$GRAMMAR" "$OUT_DIR" $option 2>&1 || true)

    # Check if expected pattern is in output
    if echo "$output" | grep -q "$expected_pattern"; then
        echo -e "${GREEN}PASS${NC}"
        ((passed++))
        return 0
    else
        echo -e "${RED}FAIL${NC} - Expected pattern '$expected_pattern' not found in output"
        ((failed++))
        return 1
    fi
}

echo "======================================"
echo "RTK Debug Options Test Suite"
echo "======================================"
echo ""
echo "Using grammar: $GRAMMAR"
echo ""

# Ensure output directory exists
mkdir -p "$OUT_DIR"

echo "=== Pipeline Stage Inspection ==="
test_option "--debug-tokens" "--debug-tokens" "LEXER OUTPUT"
test_option "--debug-parse" "--debug-parse" "PARSER OUTPUT"
test_option "--debug-string-norm" "--debug-string-norm" "String Normalization"
test_option "--debug-clause-norm" "--debug-clause-norm" "CLAUSE NORMALIZATION"
test_option "--debug-constructors" "--debug-constructors" "FINAL GRAMMAR"
echo ""

echo "=== Output Inspection ==="
test_option_file "--debug-parser-spec" "--debug-parser-spec" "$OUT_DIR/test-parser-spec.txt"
test_option_file "--debug-lexer-spec" "--debug-lexer-spec" "$OUT_DIR/test-lexer-spec.txt"
test_option_file "--debug-qq-spec" "--debug-qq-spec" "$OUT_DIR/test-qq-spec.txt"
echo ""

echo "=== Statistics and Analysis ==="
test_option "--stats" "--stats" "GRAMMAR STATISTICS"
test_option "--analyze-conflicts" "--analyze-conflicts" "CONFLICT ANALYSIS"
test_option "--show-rule-graph" "--show-rule-graph" "RULE DEPENDENCY GRAPH"
test_option "--list-rules" "--list-rules" "RULE LISTING"
echo ""

echo "=== Validation ==="
test_option "--validate-grammar" "--validate-grammar" "GRAMMAR VALIDATION"
test_option "--unused-rules" "--unused-rules" "UNUSED RULES"
test_option "--check-left-recursion" "--check-left-recursion" "LEFT RECURSION"
test_option "--suggest-shortcuts" "--suggest-shortcuts" "SHORTCUT SUGGESTIONS"
echo ""

echo "=== Selective Debug ==="
test_option_with_error "--debug-stage=lex" "--debug-stage=lex" "Stopped after requested debug stage"
test_option_with_error "--debug-stage=parse" "--debug-stage=parse" "Stopped after requested debug stage"
echo ""

echo "=== Output Format ==="
test_option "--debug-format=pretty --stats" "--debug-format=pretty --stats" "GRAMMAR STATISTICS"
test_option "--debug-format=compact --stats" "--debug-format=compact --stats" "GRAMMAR STATISTICS"
echo ""

echo "=== Performance Profiling ==="
test_option "--profile-stages" "--profile-stages" "STAGE TIMING PROFILE"
echo ""

echo "=== Combined Options ==="
test_option "--stats --validate-grammar" "--stats --validate-grammar" "GRAMMAR STATISTICS"
test_option "--list-rules --show-rule-graph" "--list-rules --show-rule-graph" "RULE LISTING"
echo ""

echo "======================================"
echo "Test Results Summary"
echo "======================================"
echo -e "Passed: ${GREEN}$passed${NC}"
echo -e "Failed: ${RED}$failed${NC}"
echo "Total:  $((passed + failed))"
echo ""

if [ $failed -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${RED}Some tests failed!${NC}"
    exit 1
fi
