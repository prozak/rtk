#!/usr/bin/env bash
# Test runner for parsing a suite of Java files
# Usage: ./test-java-suite.sh [--lex-only] [--blacklist <file>] <directory> [output-dir]

set -euo pipefail

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Parse options
LEX_ONLY=false
BLACKLIST_FILE=""

while [ "$#" -gt 0 ]; do
    case "$1" in
        --lex-only)
            LEX_ONLY=true
            shift
            ;;
        --blacklist)
            BLACKLIST_FILE="$2"
            shift 2
            ;;
        *)
            break
            ;;
    esac
done

# Check arguments
if [ $# -lt 1 ]; then
    echo "Usage: $0 [--lex-only] [--blacklist <file>] <directory> [output-dir]"
    echo "Example: $0 test-suites/commons-lang/src/main/java results/commons-lang"
    echo "Example: $0 --lex-only test-suites/commons-lang/src/main/java results/commons-lang-lex"
    echo "Example: $0 --lex-only --blacklist test-suites/commons-lang/lexer-blacklist.txt test-suites/commons-lang/src/main/java"
    exit 1
fi

JAVA_DIR="$1"
OUTPUT_DIR="${2:-test-results/$(basename "$JAVA_DIR")-$(date +%Y%m%d-%H%M%S)}"

# Check if directory exists
if [ ! -d "$JAVA_DIR" ]; then
    echo -e "${RED}Error: Directory $JAVA_DIR does not exist${NC}"
    exit 1
fi

# Use JAVA_PARSER from environment if set, otherwise default to test-out/java-main
if [ -z "${JAVA_PARSER:-}" ]; then
    # Check if Java parser exists
    if [ ! -f "test-out/java-main" ]; then
        echo -e "${YELLOW}Java parser not found. Building...${NC}"
        make test-out/java-main || {
            echo -e "${RED}Failed to build Java parser${NC}"
            exit 1
        }
    fi
    JAVA_PARSER="./test-out/java-main"
else
    echo -e "${BLUE}Using custom Java parser: $JAVA_PARSER${NC}"
fi

# Create output directory
mkdir -p "$OUTPUT_DIR"
mkdir -p "$OUTPUT_DIR/failed"
mkdir -p "$OUTPUT_DIR/succeeded"

# Load blacklist if provided
declare -A BLACKLIST
if [ -n "$BLACKLIST_FILE" ]; then
    if [ ! -f "$BLACKLIST_FILE" ]; then
        echo -e "${RED}Error: Blacklist file $BLACKLIST_FILE does not exist${NC}"
        exit 1
    fi
    echo -e "${BLUE}Loading blacklist from $BLACKLIST_FILE...${NC}"
    while IFS= read -r line; do
        # Skip comments and empty lines
        [[ "$line" =~ ^#.*$ || -z "$line" ]] && continue
        BLACKLIST["$line"]=1
    done < "$BLACKLIST_FILE"
    echo -e "${BLUE}Loaded ${#BLACKLIST[@]} blacklisted files${NC}"
fi

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
BLACKLIST_COUNT=0
declare -a FAILED_FILES
declare -a SUCCESS_FILES
declare -a BLACKLISTED_FILES

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

    # Check if file is blacklisted
    if [ -n "$BLACKLIST_FILE" ] && [ -n "${BLACKLIST[$REL_PATH]:-}" ]; then
        BLACKLIST_COUNT=$((BLACKLIST_COUNT + 1))
        BLACKLISTED_FILES+=("$REL_PATH")
        echo "$REL_PATH" >> "$OUTPUT_DIR/blacklisted.txt" 2>/dev/null || true
        continue
    fi

    # Try to parse the file (with optional --lex-only flag)
    if [ "$LEX_ONLY" = true ]; then
        PARSER_CMD="$JAVA_PARSER --lex-only $java_file"
    else
        PARSER_CMD="$JAVA_PARSER $java_file"
    fi

    if $PARSER_CMD > "$OUTPUT_DIR/tmp.out" 2>&1; then
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
TESTED_FILES=$((TOTAL_FILES - BLACKLIST_COUNT))
if [ $TESTED_FILES -gt 0 ]; then
    SUCCESS_RATE=$((SUCCESS_COUNT * 100 / TESTED_FILES))
    FAIL_RATE=$((FAIL_COUNT * 100 / TESTED_FILES))
else
    SUCCESS_RATE=0
    FAIL_RATE=0
fi

# Generate summary report
REPORT_FILE="$OUTPUT_DIR/report.txt"
MODE_DESC="Full Parse"
if [ "$LEX_ONLY" = true ]; then
    MODE_DESC="Lexical Analysis Only"
fi

cat > "$REPORT_FILE" << EOF
Java Test Suite Report
=======================
Generated: $(date)
Test Directory: $JAVA_DIR
Output Directory: $OUTPUT_DIR
Mode: $MODE_DESC

Summary Statistics
------------------
Total Files:    $TOTAL_FILES
Blacklisted:    $BLACKLIST_COUNT
Tested:         $TESTED_FILES
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
if [ $BLACKLIST_COUNT -gt 0 ]; then
    echo -e "${YELLOW}Blacklisted:    $BLACKLIST_COUNT${NC}"
    echo -e "Tested:         $TESTED_FILES"
fi
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
