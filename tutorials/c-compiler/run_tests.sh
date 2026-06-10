#!/usr/bin/env bash
# Stage-1 compiler tests, mirroring the contract of
# https://github.com/nlsandler/write_a_c_compiler:
#   - valid programs: ncc must produce an executable next to the source file
#     whose exit code matches a gcc-compiled reference
#   - invalid programs: ncc must exit non-zero and leave no executable behind
cd "$(dirname "$0")" || exit 1

if [ ! -x ./ncc ]; then
    echo "ncc not built; run 'make build' first" >&2
    exit 1
fi

fail=0

for f in tests/valid/*.c; do
    base="${f%.c}"
    rm -f "$base" "${base}_ref"
    if ! ./ncc "$f" >/dev/null 2>&1; then
        echo "FAIL  $f (compiler rejected a valid program)"
        fail=1
        continue
    fi
    "$base"
    got=$?
    gcc "$f" -o "${base}_ref" && "${base}_ref"
    want=$?
    if [ "$got" = "$want" ]; then
        echo "PASS  $f (exit code $got)"
    else
        echo "FAIL  $f (got exit code $got, gcc reference gives $want)"
        fail=1
    fi
    rm -f "$base" "${base}_ref"
done

for f in tests/invalid/*.c; do
    base="${f%.c}"
    rm -f "$base"
    if ./ncc "$f" >/dev/null 2>&1; then
        echo "FAIL  $f (compiler accepted an invalid program)"
        fail=1
    elif [ -e "$base" ] || [ -e "${base}.s" ]; then
        echo "FAIL  $f (rejected, but left artifacts behind)"
        fail=1
    else
        echo "PASS  $f (rejected)"
    fi
    rm -f "$base" "${base}.s"
done

if [ "$fail" = 0 ]; then
    echo "All stage-1 compiler tests passed."
fi
exit "$fail"
