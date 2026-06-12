#!/bin/bash
# SessionStart hook: provision the Haskell toolchain for Claude Code on the
# web sessions. The remote container's state is cached after this hook
# completes, so the expensive work runs once per environment; in later
# sessions every step below is an idempotent no-op and the hook finishes in
# seconds. Mirrors the manual setup in CLAUDE.md: apt GHC (ghcup is blocked
# here), HTTPS hackage config, alex/happy pinned to the same versions as
# .github/workflows/ci.yml, then a full cabal build.
set -euo pipefail

# Local machines manage their own toolchain; only run in remote sessions.
if [ "${CLAUDE_CODE_REMOTE:-}" != "true" ]; then
  exit 0
fi

# Hook stdout is injected into the session context, so keep it to one-line
# progress markers and send the verbose tool output to a log file.
LOG=/tmp/rtk-session-start.log
: > "$LOG"
trap 'echo "RTK session-start hook FAILED; last lines of $LOG:" >&2; tail -n 40 "$LOG" >&2' ERR

export DEBIAN_FRONTEND=noninteractive
export LANG=C.UTF-8 LC_ALL=C.UTF-8
export PATH="$HOME/.cabal/bin:$PATH"

REPO="${CLAUDE_PROJECT_DIR:-$(cd "$(dirname "$0")/../.." && pwd)}"

if ! command -v ghc >/dev/null 2>&1 || ! command -v cabal >/dev/null 2>&1; then
  echo "Installing ghc + cabal-install via apt..."
  # The container image carries PPA sources the network proxy blocks (403),
  # making apt-get update exit non-zero after the main Ubuntu archives have
  # already refreshed; ghc/cabal-install only need those, so press on and
  # let apt-get install be the real failure signal.
  apt-get update >>"$LOG" 2>&1 || true
  apt-get install -y ghc cabal-install >>"$LOG" 2>&1
fi

# Point cabal at HTTPS hackage BEFORE the first `cabal update`; the default
# HTTP URL stalls on mirror lookups (see CLAUDE.md, Common Issues).
if ! grep -qs 'url: https://hackage.haskell.org/' "$HOME/.cabal/config"; then
  mkdir -p "$HOME/.cabal"
  cat > "$HOME/.cabal/config" <<'CFG'
repository hackage.haskell.org
  url: https://hackage.haskell.org/
CFG
fi

if [ ! -e "$HOME/.cabal/packages/hackage.haskell.org/01-index.tar" ]; then
  echo "Downloading hackage package index (cabal update)..."
  cabal update >>"$LOG" 2>&1
fi

# alex/happy on PATH for the makefile targets that invoke them directly;
# same pins as ci.yml so generated artifacts match CI byte-for-byte.
if [ ! -x "$HOME/.cabal/bin/alex" ] || [ ! -x "$HOME/.cabal/bin/happy" ]; then
  echo "Installing alex-3.5.4.2 and happy-2.2..."
  cabal install alex-3.5.4.2 happy-2.2 \
    --installdir="$HOME/.cabal/bin" --overwrite-policy=always \
    --install-method=copy >>"$LOG" 2>&1
fi

# Running `cabal install` inside the project directory resolves the whole
# project plan (cheap warm-up for the build below) but also copies the
# project's own rtk executable into ~/.cabal/bin, where it would shadow
# freshly built binaries on PATH for the rest of the environment's life.
# Drop it; workflows use `cabal exec rtk --` / dist-newstyle instead.
rm -f "$HOME/.cabal/bin/rtk"

# commons-lang corpus for the test-*-commons-lang targets. Best effort: the
# build and the unit/golden suites don't need it.
if [ ! -e "$REPO/test-suites/commons-lang/.git" ]; then
  echo "Initializing commons-lang submodule..."
  git -C "$REPO" submodule update --init >>"$LOG" 2>&1 \
    || echo "WARNING: submodule init failed; commons-lang test targets won't work" >&2
fi

# Build the library, executable and both test suites so `make test` /
# `cabal test` start instantly. Dependencies come from the cached
# ~/.cabal/store; on a warm container this is a fast no-op.
echo "Building rtk (cabal build --enable-tests)..."
(cd "$REPO" && cabal build --enable-tests -j >>"$LOG" 2>&1)

# Session environment: alex/happy on PATH, UTF-8 locale for test output.
if [ -n "${CLAUDE_ENV_FILE:-}" ]; then
  {
    echo 'export PATH="$HOME/.cabal/bin:$PATH"'
    echo 'export LANG=C.UTF-8'
    echo 'export LC_ALL=C.UTF-8'
  } >> "$CLAUDE_ENV_FILE"
fi

echo "RTK toolchain ready: ghc $(ghc --numeric-version), cabal $(cabal --numeric-version), $(alex --version | head -n 1), $(happy --version | head -n 1)"
