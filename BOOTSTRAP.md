# RTK Bootstrap Self-Hosting Test

## Overview

This test tracks RTK's progress toward **self-hosting** - the ability to use RTK's own generated parsers instead of hand-written Alex and Happy specifications.

Similar to how C compilers were eventually rewritten in C, RTK can potentially use its own generated code to parse RTK grammar files.

## Current State: RTK is self-hosting in substance

RTK ships **two front ends** and both are exercised on every test run:

- **Hand-written** `Lexer.x` / `Parser.y` — the default
- **Generated** `GrammarLexer.x` / `GrammarParser.y` — RTK's own output for
  `test-grammars/grammar.pg`, compiled into rtk straight from the golden
  snapshot in `test/golden/grammar/` and selected with `--use-generated`

The snapshot is the checked-in bootstrap stage: it was produced by the
*previous* rtk binary, exactly like the stage files of any self-hosting
compiler, and `make accept-golden` refreshes it. `src/generated/ASTAdapter.hs`
converts the generated AST to the hand-written `InitialGrammar`, after which
both modes share the whole pipeline (normalization and code generation).

The fixed point holds:

```bash
cabal run rtk -- --use-generated test-grammars/grammar.pg /tmp/out
diff /tmp/out/GrammarLexer.x  test/golden/grammar/GrammarLexer.x   # identical
diff /tmp/out/GrammarParser.y test/golden/grammar/GrammarParser.y  # identical
diff /tmp/out/GrammarQQ.hs    test/golden/grammar/GrammarQQ.hs     # identical
```

RTK parses its own grammar with the parser it generated from that grammar and
regenerates that parser byte-for-byte.

### Equivalence harness

- `cabal test golden` runs **every** grammar in `test-grammars/` through both
  front ends; both must reproduce the snapshots in `test/golden/`
  byte-for-byte (except the three grammars pinned for divergences 4 and 5
  below, which are checked hand-written-only plus a still-diverges guard).
- `cabal test unit` parses every grammar with both front ends and asserts
  the `InitialGrammar`s are equal, source positions included (same three
  pinned exceptions, with the same guard).

### Known divergences (accepted and documented)

These do not affect generated artifacts for any grammar in the corpus — the
equivalence harness proves it — but they are real behavioral deltas:

1. **Lexer/parser error reporting.** The generated lexer and parser report
   `Either String` with the line/column rendered into the message text; the
   hand-written front end reports structured `Either Diagnostic` (so a
   `--use-generated` *parse* error lacks the `FILE:LINE:COL:` prefix).
   Everything after parsing is converged: generated ASTs carry the position
   of every constructor's first token, the adapter maps rule positions into
   `getIRulePos`, and diagnostics from the shared pipeline (normalization,
   generation) are identical under both front ends.
2. **Nested comments.** The generated lexer cannot lex nested
   `/* /* */ */` comments (hand-written lexer can; GitHub issue #25). No
   corpus grammar nests comments.
3. **Adjacent `"""…"""` blocks.** The hand-written path concatenates adjacent
   triple-quoted blocks (`catBigstrs`); the generated grammar accepts a
   single block after `imports`. Only `grammar.pg` uses `imports`, with one
   block, so this is theoretical today.
4. **Empty alternatives.** The hand-written parser accepts `Gd = | ExpI ;`
   (an empty first alternative, used by `test-grammars/haskell.pg`);
   grammar.pg's own clause syntax cannot derive an empty alternative, so the
   generated front end rejects the file. One of the two definitions of the
   grammar language has to win here — follow-up work.
5. **Redundant parentheses are grouping to the hand-written parser.**
   `(ImportStatement)*` (java.pg) and `(A B) C` (t1.pg) parse to nested
   `IAlt [ISeq …]` groups that normalize into extra proxy sub-rules;
   grammar.pg's `Clause5 = '(' ,Clause ')'` lifts the group, so the parens
   are absent from the generated AST and the artifacts genuinely differ.

Because of 4 and 5, three grammars (`haskell`, `java`, `t1`) are pinned in
`test/TestSupport.hs` (`frontEndDivergentGrammars`): the golden suite checks
them with the hand-written front end only, and both suites fail as soon as a
pinned grammar stops diverging so the pin gets dropped. Every other grammar
in the corpus — including `grammar.pg` itself — passes the strict
dual-front-end equivalence.

The original comparison test below remains useful for tracking *textual*
convergence of the generated `.x`/`.y` with the hand-written ones — full
textual identity is not the goal anymore, behavioral equivalence is.

## Running the Test

### Locally

```bash
# Build RTK and generate grammar files
make build
make test-grammar

# Run the bootstrap comparison
make test-bootstrap
```

### In CI

The test runs automatically in GitHub Actions after all other tests. It's marked as **informational only** (`continue-on-error: true`), so differences won't fail the build.

## What the Test Does

The `compare-bootstrap.sh` script:

1. **Generates** files from `test-grammars/grammar.pg`:
   - `test-out/GrammarLexer.x`
   - `test-out/GrammarParser.y`
   - `test-out/GrammarQQ.hs`

2. **Compares** generated files with hand-written ones:
   - `Lexer.x` ↔ `test-out/GrammarLexer.x`
   - `Parser.y` ↔ `test-out/GrammarParser.y`

3. **Reports** differences (two-level check):
   - First: Exact match check
   - Second: Content match (using `diff -uwB` to ignore whitespace and blank lines)
   - ✓ Green: Files are identical (self-hosting achieved!)
   - ℹ Blue: Only whitespace differences (content is identical)
   - ⚠ Yellow: Content differences found (expected during development)
   - Shows line counts and diff statistics

## Interpreting Results

### If files are identical (✓)
RTK is fully self-hosting! The generated code matches hand-written code exactly.

### If only whitespace differs (ℹ)
The content is functionally identical! Only whitespace, indentation, or blank lines differ. This is excellent progress toward self-hosting.

### If files differ (⚠)
This is **expected** and shows what needs to be addressed:

- **Formatting differences**: Auto-generated code may format differently
- **Comments**: Hand-written files may have more documentation
- **Optimizations**: Hand-written code may include manual optimizations
- **Features**: Grammar may not yet capture all hand-written features

### Diff Options Used

The comparison uses `diff -uwB` for content checking:
- `-u`: Unified diff format (shows context)
- `-w`: Ignore all whitespace differences (spaces, tabs)
- `-B`: Ignore blank line changes

This focuses the comparison on actual semantic differences rather than formatting.

## Path to Self-Hosting

1. ✅ **Verify grammar completeness**: `test-grammars/grammar.pg` parses the
   corpus, surfacing exactly two constructs the hand-written parser supports
   beyond the spec (divergences 4 and 5 above)
2. ✅ **Test equivalence**: both front ends reproduce identical artifacts for
   every corpus grammar except the three pinned divergent ones
3. ✅ **Dual-mode entry point**: `--use-generated` switches `main.hs` to the
   generated front end
4. ✅ **Bootstrap cycle**: `rtk --use-generated grammar.pg` regenerates its own
   parser byte-for-byte (the fixed point)
5. ✅ **Structured positions in the generated path**: every generated AST
   constructor carries the position of its first token (equality-transparent
   `RtkPos`), the adapter maps rule positions into `getIRulePos`, and the
   AST equality suite compares positions too
6. **Retire hand-written files**: make generated mode the default, keep
   `Lexer.x`/`Parser.y` as reference

## Benefits of Self-Hosting

- **Dogfooding**: RTK uses its own capabilities
- **Simplified maintenance**: Update `grammar.pg` instead of editing `.x`/`.y` files
- **QuasiQuoter support**: Get compile-time code generation for grammar manipulation
- **Feature validation**: Every RTK feature must work on RTK's own grammar

## Files

- `compare-bootstrap.sh` - Comparison script
- `test-grammars/grammar.pg` - RTK's grammar definition
- `Lexer.x` - Hand-written lexer (current)
- `Parser.y` - Hand-written parser (current)
- `.github/workflows/ci.yml` - CI configuration (runs test automatically)
- `makefile` - Build system (includes `test-bootstrap` target)

## Further Reading

- [Bootstrapping Compilers](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
- Self-hosting examples: GCC, Rust compiler, TypeScript
