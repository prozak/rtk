#!/usr/bin/env bash
# Test runner for parsing a suite of Java files
# Usage: ./test-java-suite.sh <directory> [output-dir]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 <directory> [output-dir]"
    echo "Example: $0 test-suites/commons-lang/src/main/java results/commons-lang"
    exit 1
fi

JAVA_DIR="$1"
OUTPUT_DIR="${2:-test-results/$(basename "$JAVA_DIR")-$(date +%Y%m%d-%H%M%S)}"

# Check if directory exists
if [ ! -d "$JAVA_DIR" ]; then
    echo -e "${RED}Error: Directory $JAVA_DIR does not exist${NC}"
    exit 1
fi

# Check if Java parser exists
if [ ! -f "test-out/java-main" ]; then
    echo -e "${YELLOW}Java parser not found. Building...${NC}"
    make test-out/java-main || {
        echo -e "${RED}Failed to build Java parser${NC}"
        exit 1
    }
fi

JAVA_PARSER="./test-out/java-main"

# Create output directory
mkdir -p "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR/failed"
mkdir -p "$OUTPUT_DIR/succeeded"

# Find all Java files
echo -e "${BLUE}Finding Java files in $JAVA_DIR...${NC}"
mapfile -t JAVA_FILES < <(find "$JAVA_DIR" -name "*.java" -type f | sort)
TOTAL_FILES=${#JAVA_FILES[@]}

if [ $TOTAL_FILES -eq 0 ]; then
    echo -e "${RED}No Java files found in $JAVA_DIR${NC}"
    exit 1
fi

echo -e "${BLUE}Found $TOTAL_FILES Java files${NC}"
echo ""

# Initialize counters
SUCCESS_COUNT=0
FAIL_COUNT=0
declare -a FAILED_FILES
declare -a SUCCESS_FILES

# Progress bar function
progress_bar() {
    local current=$1
    local total=$2
    local width=50
    local percentage=$((current * 100 / total))
    local filled=$((width * current / total))
    local empty=$((width - filled))

    printf "\r["
    printf "%${filled}s" | tr ' ' '='
    printf "%${empty}s" | tr ' ' ' '
    printf "] %3d%% (%d/%d)" "$percentage" "$current" "$total"
}

# Test each file
echo -e "${BLUE}Parsing files...${NC}"
CURRENT=0

for java_file in "${JAVA_FILES[@]}"; do
    CURRENT=$((CURRENT + 1))
    progress_bar $CURRENT $TOTAL_FILES

    # Get relative path for reporting
    REL_PATH="${java_file#$JAVA_DIR/}"

    # Try to parse the file
    if $JAVA_PARSER "$java_file" > "$OUTPUT_DIR/tmp.out" 2>&1; then
        SUCCESS_COUNT=$((SUCCESS_COUNT + 1))
        SUCCESS_FILES+=("$REL_PATH")
        echo "$REL_PATH" >> "$OUTPUT_DIR/succeeded/list.txt"
    else
        FAIL_COUNT=$((FAIL_COUNT + 1))
        FAILED_FILES+=("$REL_PATH")

        # Save error output
        ERROR_FILE="$OUTPUT_DIR/failed/${REL_PATH//\//_}.err"
        cp "$OUTPUT_DIR/tmp.out" "$ERROR_FILE"
        echo "$REL_PATH" >> "$OUTPUT_DIR/failed/list.txt"
    fi
done

echo ""
echo ""

# Calculate statistics
SUCCESS_RATE=$((SUCCESS_COUNT * 100 / TOTAL_FILES))
FAIL_RATE=$((FAIL_COUNT * 100 / TOTAL_FILES))

# Generate summary report
REPORT_FILE="$OUTPUT_DIR/report.txt"
cat > "$REPORT_FILE" << EOF
Java Test Suite Report
=======================
Generated: $(date)
Test Directory: $JAVA_DIR
Output Directory: $OUTPUT_DIR

Summary Statistics
------------------
Total Files:    $TOTAL_FILES
Successful:     $SUCCESS_COUNT ($SUCCESS_RATE%)
Failed:         $FAIL_COUNT ($FAIL_RATE%)

EOF

# Add failed files to report if any
if [ $FAIL_COUNT -gt 0 ]; then
    cat >> "$REPORT_FILE" << EOF
Failed Files
------------
EOF
    printf '%s\n' "${FAILED_FILES[@]}" >> "$REPORT_FILE"
fi

# Display summary
echo -e "${BLUE}======================================${NC}"
echo -e "${BLUE}Test Suite Results${NC}"
echo -e "${BLUE}======================================${NC}"
echo -e "Total Files:    $TOTAL_FILES"
echo -e "${GREEN}Successful:     $SUCCESS_COUNT ($SUCCESS_RATE%)${NC}"
if [ $FAIL_COUNT -gt 0 ]; then
    echo -e "${RED}Failed:         $FAIL_COUNT ($FAIL_RATE%)${NC}"
else
    echo -e "${GREEN}Failed:         $FAIL_COUNT ($FAIL_RATE%)${NC}"
fi
echo ""
echo -e "Full report saved to: ${BLUE}$REPORT_FILE${NC}"
echo -e "Failed file list: ${BLUE}$OUTPUT_DIR/failed/list.txt${NC}"
echo -e "Successful file list: ${BLUE}$OUTPUT_DIR/succeeded/list.txt${NC}"
echo ""

# Show first few failures
if [ $FAIL_COUNT -gt 0 ] && [ $FAIL_COUNT -le 10 ]; then
    echo -e "${YELLOW}Failed files:${NC}"
    printf '  - %s\n' "${FAILED_FILES[@]}"
    echo ""
elif [ $FAIL_COUNT -gt 10 ]; then
    echo -e "${YELLOW}First 10 failed files:${NC}"
    printf '  - %s\n' "${FAILED_FILES[@]:0:10}"
    echo -e "  ... and $((FAIL_COUNT - 10)) more"
    echo ""
fi

# Clean up
rm -f "$OUTPUT_DIR/tmp.out"

# Exit with appropriate code
if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}All tests passed!${NC}"
    exit 0
else
    echo -e "${YELLOW}Some tests failed. Check the report for details.${NC}"
    exit 1
fi
