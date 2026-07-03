# Releasing rtk to Hackage

The protocol that shipped 0.11 and 0.12 (2026-07). Every step was needed at
least once; the caveats at the bottom all actually happened.

## 1. Release-prep PR

- `rtk.cabal`: confirm the version; widen dependency bounds only to what CI
  actually tests (the pinned toolchain and the newest-GHC canary are the
  endpoints — check the canary's GHC release for its boot-library versions).
  Bounds are stated ONCE per dependency (Hackage trustee guidance); the
  executable and test suites use bare names for packages the libraries bound.
- `CHANGELOG.md`: retitle `[Unreleased]` to `[<version>] - <date>`.
- Merge via PR with CI green, then tag: `git tag -a v<V> <sha> && git push
  origin v<V>`.

## 2. Pre-flight

```bash
make release-check
```

Runs `cabal check`, builds the sdist, rebuilds + runs both cabal test suites
from the unpacked tarball, and builds Hackage-format haddocks. Also eyeball
the tarball for release-specific files: `tar tzf .../rtk-<V>.tar.gz`.

## 3. Candidate → review → publish

`cabal upload` hangs on this machine (its curl invocation stalls; see
caveats) — upload with curl directly. `-u <user>` with no password prompts
interactively; basic auth over HTTPS is fine and accepted by Hackage.

```bash
# candidate (deletable, re-uploadable — mistakes are free here)
curl -u <hackage-user> -F "package=@rtk-<V>.tar.gz" \
     https://hackage.haskell.org/packages/candidates/

# review https://hackage.haskell.org/package/rtk-<V>/candidate :
#   bounds render, changelog top section, module list, zero page warnings

# publish (PERMANENT — a published version can never be re-uploaded)
curl -u <hackage-user> -X POST \
     https://hackage.haskell.org/package/rtk-<V>/candidate/publish
```

## 4. Docs

Hackage's doc builder cannot build rtk (its nix environment ships happy
1.20.x; rtk pins `happy == 2.2.*`), so the build report stays red and docs
must be self-uploaded — the tarball from `make release-check`:

```bash
curl -u <hackage-user> -X PUT --data-binary @rtk-<V>-docs.tar.gz \
     -H "Content-Type: application/x-tar" -H "Content-Encoding: gzip" \
     https://hackage.haskell.org/package/rtk-<V>/docs
```

## 5. Verify and close out

- `cabal update && cabal install rtk-<V>` from a NEUTRAL directory (inside
  the repo, the local package shadows Hackage). The index can lag the publish
  by a few minutes — rerun `cabal update` if the version is missing.
- Smoke test the installed binary; the strongest check is the self-hosting
  fixed point: `rtk test-grammars/grammar.pg out/` (tag-era copy) must
  reproduce `test/golden/grammar/` byte-for-byte.
- Post-release PR: bump `rtk.cabal` to the next dev version, reopen
  `[Unreleased]` in CHANGELOG.md, update CLAUDE.md's version line.

## Caveats that actually happened

- **`cabal upload` (3.8) hangs for ~9 minutes** then fails with curl error
  56: its curl child sends the request headers and waits for a server
  response that never comes (the tarball body is never transmitted). Direct
  curl against the same endpoint completes in ~1 second.
- **Index lag**: `cabal update` run within a minute of publishing did not see
  the new version; a later update did.
- **Local package shadowing**: `cabal install rtk-<V>` run from inside the
  repo tries to satisfy `rtk` from the working tree and fails on the version
  mismatch.
- **Doc builder**: report says `Install: PlanningFailed` (bounds exclude its
  GHC) or `ConfigureFailed` (happy too old). Reports are immutable history;
  they do not update after a docs upload, and that is fine.
