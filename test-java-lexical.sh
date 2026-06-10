#!/usr/bin/env bash
# Golden tests for the RTK-generated Java lexer.
#
# For each test-grammars/java/lexical/*.java file, dump the exact token
# stream with `java-main --dump-tokens` and compare it against the sibling
# .tokens golden file. Unlike --lex-only (which only detects characters that
# match no rule at all), this catches mis-tokenization: a literal that lexes
# as the wrong token, or silently splits into several tokens.
#
# Usage: ./test-java-lexical.sh
#        ACCEPT=1 ./test-java-lexical.sh   # regenerate goldens, then review diff

set -euo pipefail

RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m'

DRIVER="${JAVA_PARSER:-./test-out/java-main}"
CORPUS_DIR="test-grammars/java/lexical"

if [ ! -x "$DRIVER" ]; then
    echo -e "${RED}Java lexer driver not found: $DRIVER${NC}"
    echo "Build it first: make test-out/java-main"
    exit 1
fi

FAIL_COUNT=0
PASS_COUNT=0

for java_file in "$CORPUS_DIR"/*.java; do
    golden="${java_file%.java}.tokens"

    if ! actual=$("$DRIVER" --dump-tokens "$java_file" 2>&1); then
        echo -e "${RED}FAIL${NC} $java_file (lexer error)"
        echo "$actual" | sed 's/^/    /'
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi

    if [ "${ACCEPT:-0}" = "1" ]; then
        printf '%s\n' "$actual" > "$golden"
        echo -e "${YELLOW}ACCEPT${NC} $golden"
        continue
    fi

    if [ ! -f "$golden" ]; then
        echo -e "${RED}FAIL${NC} $java_file (missing golden file $golden)"
        echo "    Run: ACCEPT=1 ./test-java-lexical.sh and review the new file"
        FAIL_COUNT=$((FAIL_COUNT + 1))
        continue
    fi

    if diff_out=$(diff -u "$golden" <(printf '%s\n' "$actual")); then
        echo -e "${GREEN}PASS${NC} $java_file"
        PASS_COUNT=$((PASS_COUNT + 1))
    else
        echo -e "${RED}FAIL${NC} $java_file (token stream differs from golden)"
        echo "$diff_out" | sed 's/^/    /'
        FAIL_COUNT=$((FAIL_COUNT + 1))
    fi
done

echo ""
if [ "${ACCEPT:-0}" = "1" ]; then
    echo -e "${YELLOW}Golden files regenerated. Review with: git diff $CORPUS_DIR${NC}"
    exit 0
fi

if [ $FAIL_COUNT -eq 0 ]; then
    echo -e "${GREEN}All $PASS_COUNT lexical golden tests passed!${NC}"
    exit 0
else
    echo -e "${RED}$FAIL_COUNT lexical golden test(s) failed ($PASS_COUNT passed)${NC}"
    exit 1
fi
