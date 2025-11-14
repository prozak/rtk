#!/usr/bin/env bash
# Test all manual Java test files with the full parser

set -euo pipefail

# Find all Java test files
JAVA_FILES=$(find test-grammars/java -name "*.java" -type f | sort)

PASSED=0
FAILED=0
TOTAL=0

declare -a PASSED_FILES
declare -a FAILED_FILES

echo "Testing all Java test files with full parser..."
echo "================================================"
echo ""

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
        mkdir -p test-out/parser-failures
        ERROR_FILE="test-out/parser-failures/$(basename "$file" .java).err"
        ./test-out/java-main-full "$file" > "$ERROR_FILE" 2>&1 || true
    fi
done

echo ""
echo "================================================"
echo "Test Results Summary"
echo "================================================"
echo "Total:  $TOTAL"
echo "Passed: $PASSED ($((PASSED * 100 / TOTAL))%)"
echo "Failed: $FAILED ($((FAILED * 100 / TOTAL))%)"
echo ""

if [ $FAILED -gt 0 ]; then
    echo "Failed files:"
    printf '  - %s\n' "${FAILED_FILES[@]}"
    echo ""
    echo "Error details saved in test-out/parser-failures/"
fi

if [ $PASSED -gt 0 ]; then
    echo "Passed files:"
    printf '  - %s\n' "${PASSED_FILES[@]}"
fi

echo ""
exit $([ $FAILED -eq 0 ] && echo 0 || echo 1)
