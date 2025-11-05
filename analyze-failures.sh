#!/usr/bin/env bash
# Analyze common failure patterns in test results
# Usage: ./analyze-failures.sh <test-results-dir>

set -euo pipefail

# Colors
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
CYAN='\033[0;36m'
NC='\033[0m'

if [ $# -lt 1 ]; then
    echo "Usage: $0 <test-results-dir>"
    echo "Example: $0 test-results/main-java-20231104-120000"
    exit 1
fi

RESULTS_DIR="$1"
FAILED_DIR="$RESULTS_DIR/failed"

if [ ! -d "$FAILED_DIR" ]; then
    echo -e "${RED}Error: Failed directory not found: $FAILED_DIR${NC}"
    exit 1
fi

echo -e "${BLUE}=====================================${NC}"
echo -e "${BLUE}Java Test Failure Analysis${NC}"
echo -e "${BLUE}=====================================${NC}"
echo ""

# Count total failures
TOTAL_FAILURES=$(find "$FAILED_DIR" -name "*.err" -type f | wc -l)
echo -e "Total failures: ${RED}$TOTAL_FAILURES${NC}"
echo ""

if [ $TOTAL_FAILURES -eq 0 ]; then
    echo -e "${GREEN}No failures to analyze!${NC}"
    exit 0
fi

# Common error patterns to search for
declare -A ERROR_PATTERNS=(
    ["Lambda expressions"]="-> |lambda"
    ["Method references"]="::"
    ["Try-with-resources"]="try.*\(.*=.*\)"
    ["Enhanced for-loop"]="for.*:.*\)"
    ["Diamond operator"]="<>"
    ["var keyword"]="var "
    ["Records"]="record "
    ["Sealed classes"]="sealed "
    ["Pattern matching"]="instanceof.*\("
    ["Text blocks"]='"""'
    ["Switch expressions"]="switch.*->"
    ["Parse errors"]="parse error|Parse error|syntax error"
    ["Lexer errors"]="lexical error|Lexer error"
    ["Unexpected token"]="unexpected|Unexpected"
)

echo -e "${CYAN}Common Error Patterns:${NC}"
echo ""

# Analyze error patterns
for pattern_name in "${!ERROR_PATTERNS[@]}"; do
    pattern="${ERROR_PATTERNS[$pattern_name]}"
    count=$(grep -Erl "$pattern" "$FAILED_DIR" 2>/dev/null | wc -l)

    if [ $count -gt 0 ]; then
        percentage=$((count * 100 / TOTAL_FAILURES))
        printf "  %-25s: %3d files (%2d%%)\n" "$pattern_name" "$count" "$percentage"
    fi
done

echo ""

# Find most common error messages
echo -e "${CYAN}Most Common Error Messages:${NC}"
echo ""
grep -h "error\|Error\|ERROR" "$FAILED_DIR"/*.err 2>/dev/null | \
    sed 's/line [0-9]*/line N/g' | \
    sed 's/column [0-9]*/column N/g' | \
    sort | uniq -c | sort -rn | head -10 | \
    awk '{$1=$1; printf "  %4d: %s\n", $1, substr($0, index($0,$2))}'

echo ""

# Sample failures for each major category
echo -e "${CYAN}Sample Failures by Category:${NC}"
echo ""

for pattern_name in "Lambda expressions" "Method references" "Enhanced for-loop" "Parse errors"; do
    if [ -n "${ERROR_PATTERNS[$pattern_name]}" ]; then
        pattern="${ERROR_PATTERNS[$pattern_name]}"
        sample=$(grep -Erl "$pattern" "$FAILED_DIR" 2>/dev/null | head -1)

        if [ -n "$sample" ]; then
            filename=$(basename "$sample" .err)
            echo -e "${YELLOW}[$pattern_name]${NC} Example: $filename"

            # Show first error line
            grep -E "$pattern|error|Error" "$sample" 2>/dev/null | head -3 | \
                sed 's/^/    /'
            echo ""
        fi
    fi
done

# Generate recommendations
echo -e "${BLUE}=====================================${NC}"
echo -e "${BLUE}Recommendations${NC}"
echo -e "${BLUE}=====================================${NC}"
echo ""

# Check for modern Java features
modern_features=0
for feature in "Lambda expressions" "Method references" "Enhanced for-loop" "var keyword" "Records" "Sealed classes"; do
    pattern="${ERROR_PATTERNS[$feature]}"
    count=$(grep -Erl "$pattern" "$FAILED_DIR" 2>/dev/null | wc -l)
    if [ $count -gt 0 ]; then
        modern_features=$((modern_features + count))
    fi
done

if [ $modern_features -gt 0 ]; then
    percentage=$((modern_features * 100 / TOTAL_FAILURES))
    echo -e "${YELLOW}• $modern_features failures ($percentage%) are due to modern Java features${NC}"
    echo "  Consider implementing:"
    [ $(grep -Erl "${ERROR_PATTERNS["Lambda expressions"]}" "$FAILED_DIR" 2>/dev/null | wc -l) -gt 0 ] && echo "    - Lambda expressions (->)"
    [ $(grep -Erl "${ERROR_PATTERNS["Method references"]}" "$FAILED_DIR" 2>/dev/null | wc -l) -gt 0 ] && echo "    - Method references (::)"
    [ $(grep -Erl "${ERROR_PATTERNS["Enhanced for-loop"]}" "$FAILED_DIR" 2>/dev/null | wc -l) -gt 0 ] && echo "    - Enhanced for-loop (for-each)"
    [ $(grep -Erl "${ERROR_PATTERNS["var keyword"]}" "$FAILED_DIR" 2>/dev/null | wc -l) -gt 0 ] && echo "    - var keyword"
    echo ""
fi

# Check for parse errors
parse_errors=$(grep -Erl "parse error|Parse error|syntax error" "$FAILED_DIR" 2>/dev/null | wc -l)
if [ $parse_errors -gt 0 ]; then
    percentage=$((parse_errors * 100 / TOTAL_FAILURES))
    echo -e "${YELLOW}• $parse_errors failures ($percentage%) are parse/syntax errors${NC}"
    echo "  These may indicate grammar issues or edge cases"
    echo ""
fi

echo -e "${GREEN}Full results available in: $RESULTS_DIR${NC}"
