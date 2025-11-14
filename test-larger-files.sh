#!/usr/bin/env bash
# Test larger Java files with the full parser

set -euo pipefail

JAVA_FILES=$(find test-grammars -maxdepth 1 -name "*.java" -type f | sort)

PASSED=0
FAILED=0
TOTAL=0

declare -a PASSED_FILES
declare -a FAILED_FILES

echo "Testing larger Java files with full parser..."
echo "=============================================="
echo ""

mkdir -p test-out/parser-failures

for file in $JAVA_FILES; do
    TOTAL=$((TOTAL + 1))
    echo -n "Testing $file ... "

    if ./test-out/java-main-full "$file" > /dev/null 2>&1; then
        echo "✓ PASS"
        PASSED=$((PASSED + 1))
        PASSED_FILES+=("$file")
    else
        echo "✗ FAIL"
        FAILED=$((FAILED + 1))
        FAILED_FILES+=("$file")

        # Save error for analysis
        ERROR_FILE="test-out/parser-failures/$(basename "$file" .java).err"
        ./test-out/java-main-full "$file" > "$ERROR_FILE" 2>&1 || true
    fi
done

echo ""
echo "=============================================="
echo "Test Results Summary"
echo "=============================================="
echo "Total:  $TOTAL"
echo "Passed: $PASSED"
echo "Failed: $FAILED"
echo ""

if [ $FAILED -gt 0 ]; then
    echo "Failed files:"
    printf '  - %s\n' "${FAILED_FILES[@]}"
fi

if [ $PASSED -gt 0 ]; then
    echo ""
    echo "Passed files:"
    printf '  - %s\n' "${PASSED_FILES[@]}"
fi

echo ""
