#!/usr/bin/env bash
# Parser ("validator") tests, the deliverable of parts 1-3 of the tutorial:
#   - valid programs: pl0 must accept them (exit 0)
#   - invalid programs: pl0 must exit non-zero with a positioned diagnostic
#     (every error message names a line number)
cd "$(dirname "$0")" || exit 1

if [ ! -x ./pl0 ]; then
    echo "pl0 not built; run 'make build' first" >&2
    exit 1
fi

fail=0

for f in tests/valid/*.pl0; do
    if ./pl0 "$f" >/dev/null 2>&1; then
        echo "PASS  $f"
    else
        echo "FAIL  $f (validator rejected a valid program)"
        fail=1
    fi
done

for f in tests/invalid/*.pl0; do
    err=$(./pl0 "$f" 2>&1 >/dev/null)
    if ./pl0 "$f" >/dev/null 2>&1; then
        echo "FAIL  $f (validator accepted an invalid program)"
        fail=1
    elif ! printf '%s' "$err" | grep -q "line"; then
        echo "FAIL  $f (rejected, but the diagnostic has no position: $err)"
        fail=1
    else
        echo "PASS  $f (rejected: $err)"
    fi
done

if [ "$fail" = 0 ]; then
    echo "All PL/0 parser tests passed."
fi
exit "$fail"
