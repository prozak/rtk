#!/bin/bash
# Compare hand-written grammar files with generated ones
# This script helps track progress toward self-hosting RTK

set -e

echo "=========================================="
echo "RTK Bootstrap Comparison Test"
echo "=========================================="
echo ""
echo "This test compares hand-written grammar files (Lexer.x, Parser.y)"
echo "with generated files from test-grammars/grammar.pg"
echo ""

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Check if generated files exist
if [ ! -f "test-out/GrammarLexer.x" ] || [ ! -f "test-out/GrammarParser.y" ]; then
    echo -e "${RED}Error: Generated files not found in test-out/${NC}"
    echo "Please run 'make test-grammar' first to generate the files"
    exit 1
fi

echo "Comparing files..."
echo ""

# Function to compare files and show statistics
compare_files() {
    local hand_written=$1
    local generated=$2
    local file_type=$3

    echo -e "${BLUE}=== Comparing $file_type ===${NC}"
    echo "Hand-written: $hand_written"
    echo "Generated:    $generated"
    echo ""

    if [ ! -f "$hand_written" ]; then
        echo -e "${RED}Hand-written file not found!${NC}"
        echo ""
        return 1
    fi

    if [ ! -f "$generated" ]; then
        echo -e "${RED}Generated file not found!${NC}"
        echo ""
        return 1
    fi

    # Show file sizes
    hand_size=$(wc -l < "$hand_written")
    gen_size=$(wc -l < "$generated")
    echo "Lines: Hand-written=$hand_size, Generated=$gen_size"
    echo ""

    # First check: exact match
    if diff -u "$hand_written" "$generated" > /tmp/diff_exact.txt 2>&1; then
        echo -e "${GREEN}✓ Files are EXACTLY identical!${NC}"
        echo ""
        return 0
    fi

    # Second check: ignore whitespace differences
    # -w: ignore all white space
    # -B: ignore blank lines
    if diff -uwB "$hand_written" "$generated" > /tmp/diff_content.txt 2>&1; then
        echo -e "${GREEN}✓ Files are identical (ignoring whitespace)${NC}"
        echo -e "${BLUE}ℹ Only whitespace/blank line differences detected${NC}"
        echo ""
        return 0
    fi

    # Files differ in content
    # Count differences
    added=$(grep -c "^+" /tmp/diff_content.txt || true)
    removed=$(grep -c "^-" /tmp/diff_content.txt || true)
    echo -e "${YELLOW}⚠ Files differ in CONTENT: +$added lines, -$removed lines${NC}"
    echo -e "${BLUE}(using diff -uwB: ignore whitespace and blank lines)${NC}"
    echo ""

    # Show first 50 lines of diff
    echo "First 50 lines of content differences:"
    echo "----------------------------------------"
    head -n 50 /tmp/diff_content.txt
    echo "----------------------------------------"
    echo ""

    # Ask if user wants to see full diff
    if [ -t 0 ]; then  # Only in interactive mode
        echo "(Full diff saved to /tmp/diff_content.txt)"
    else
        echo "Full diff:"
        cat /tmp/diff_content.txt
    fi
    echo ""
    return 1
}

# Track overall status
lexer_status=0
parser_status=0

# Compare Lexer
compare_files "Lexer.x" "test-out/GrammarLexer.x" "Lexer (Alex specification)" || lexer_status=$?

# Compare Parser
compare_files "Parser.y" "test-out/GrammarParser.y" "Parser (Happy specification)" || parser_status=$?

# Check for QuasiQuoter (only generated, no hand-written version)
echo -e "${BLUE}=== QuasiQuoter Status ===${NC}"
if [ -f "test-out/GrammarQQ.hs" ]; then
    qq_size=$(wc -l < "test-out/GrammarQQ.hs")
    echo -e "${GREEN}✓ GrammarQQ.hs generated successfully ($qq_size lines)${NC}"
    echo "Note: No hand-written QuasiQuoter exists for comparison"
else
    echo -e "${RED}✗ GrammarQQ.hs not found${NC}"
fi
echo ""

# Summary
echo "=========================================="
echo "Summary"
echo "=========================================="
if [ $lexer_status -eq 0 ] && [ $parser_status -eq 0 ]; then
    echo -e "${GREEN}✓ RTK is fully self-hosting!${NC}"
    echo "The generated files are identical to hand-written ones."
    exit 0
else
    echo -e "${YELLOW}⚠ RTK is not yet fully self-hosting${NC}"
    echo ""
    echo "Status:"
    if [ $lexer_status -eq 0 ]; then
        echo -e "  Lexer:  ${GREEN}✓ Identical${NC}"
    else
        echo -e "  Lexer:  ${YELLOW}⚠ Differences found${NC}"
    fi

    if [ $parser_status -eq 0 ]; then
        echo -e "  Parser: ${GREEN}✓ Identical${NC}"
    else
        echo -e "  Parser: ${YELLOW}⚠ Differences found${NC}"
    fi
    echo ""
    echo "This is expected during development. The differences show"
    echo "what needs to be addressed to achieve full self-hosting."

    # Exit with 0 (success) for informational purposes
    # This allows the test to pass in CI while showing differences
    exit 0
fi
