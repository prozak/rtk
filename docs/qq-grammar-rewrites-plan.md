# Plan: grammar rewrites with RTK's own QQ — migrating the pipeline off `InitialGrammar`

Status: 8a DONE (named constructors — leading-label syntax `Add: Expr '+'
Term`, grammar.pg fully labeled, `ASTAdapter` free of `Ctr__*` references);
8b DONE (GrammarQQ compiled into rtk); 8c, 8d PLANNED (follow-up to the
default-front-end flip, PR #112).

The flip made grammar.pg authoritative and the generated front end the
default, but the pipeline still computes over the original hand-written data
structures: `ASTAdapter.convertGrammar` converts the generated AST (the
`Ctr__*` constructors from `GrammarParser`) into `Syntax.InitialGrammar`
immediately after parsing, and `Normalize`/`GenX`/`GenY`/`GenQ` operate on
`InitialGrammar`/`NormalGrammar`. `GrammarQQ.hs` is now compiled into rtk
(task 8b), but its quoters produce the *generated* AST types, not the
`InitialGrammar`/`NormalGrammar` the pipeline computes over — so rewrites
still cannot be written as `[rule| … |]` / `[cl| … |]` quasi-quoted patterns
inside the pipeline until 8c migrates it.

This file sketches that migration as four self-contained task blobs (8a–8d).
Each blob is written to be pasted into a fresh session as the task
description. They are ordered by dependency: 8a unblocks ergonomics, 8b
unblocks QQ-in-rtk, 8c is the structural migration, 8d is the payoff.
8a and 8b are independent of each other.

Invariants every task must preserve:

- The bootstrap fixed point: `rtk test-grammars/grammar.pg out/` reproduces
  `test/golden/grammar/` byte-for-byte (see BOOTSTRAP.md).
- `cabal build --ghc-options=-Werror --enable-tests` clean; `cabal test`
  green; the make battery (`test-grammar`, `test-all-java`, `test-lex-java`,
  `test-java-qq`, `test-debug-options`) green.
- Golden changes go through `make accept-golden` with the diff reviewed.
  Changes that alter the *generated grammar front end itself* (grammar.pg or
  generator output for it) are two-phase: accept the snapshot, rebuild (the
  `generated-frontend` cabal component recompiles from the snapshot), re-run
  the suites, re-check the fixed point.

---

## Task 8a — Named constructors in grammar.pg (ergonomics prerequisite)

### TL;DR

The generated AST's constructors are positional (`Ctr__Clause__5`,
`Ctr__Rule__2`, …): they encode the index of the alternative inside the rule,
so any reordering or insertion in grammar.pg silently renames constructors.
Code (and QQ patterns) written against the generated AST would be brittle and
unreadable. Add an opt-in way to NAME an alternative's constructor in the
grammar language, use it throughout grammar.pg, regenerate. This is also a
user-facing rtk feature, valuable independently of the migration.

### Current mechanics (where names come from today)

- `Normalize.fillConstructorNames` fills the `ConstructorName` field of
  `Syntax.STSeq` for every alternative that doesn't already have one;
  generated names are `Ctr__<RuleName>__<index>` (see `Normalize.hs`).
- `GenAST.genAST` emits the `data` declarations from those `STSeq` names;
  `GenY` references them in semantic actions; `GenQ`'s `Anti_*` machinery
  has its own naming (`isAntiConstructor` in `GenAST`).
- The grammar language already has per-RULE annotations (`@shortcuts(...)`,
  `@symmacro` — see `Option` in grammar.pg and `IOption` in `Syntax.hs`) but
  nothing per-ALTERNATIVE.

### Design decision (make it early, in the session)

Pick the surface syntax for naming an alternative. Two candidates:

1. Leading label on an alternative: `Clause: Clause4 = Star: Clause5 '*'
   OptDelim | Plus: Clause5 '+' OptDelim | …` — reads like Haskell data
   declarations, but `Name ':'` is ambiguous with the existing
   `Type '.' Func ':' Name '='` rule-header forms; check LALR feasibility in
   both grammar.pg and the reference Parser.y before committing to it.
2. A trailing annotation token, e.g. `Clause5 '*' OptDelim @ctor(Star)` —
   unambiguous (new `@ctor` keyword parallels `@shortcuts`), slightly noisier.

Whichever wins must be expressible in grammar.pg itself (the language is
self-describing) and parsed identically by the reference parser.

### Steps

1. Extend `Syntax.IClause`/rule structures to carry the optional name through
   parsing (likely a new field or wrapper on the alternative level; mind that
   `ISeq`/`IAlt` shapes are pinned by the dual-front-end AST equality suite —
   both front ends must represent the annotation identically).
2. grammar.pg: add the syntax to the spec; reference `Parser.y` follows;
   `ASTAdapter` maps the new constructor(s).
3. `Normalize.fillConstructorNames`: a user-supplied name wins; collision
   checks (duplicate explicit names, clash with generated `Ctr__*`/`Anti_*`)
   become diagnostics with positions.
4. Name every alternative in grammar.pg with stable, meaningful names
   (`Star`, `Plus`, `Opt`, `Group`, `RuleSimple`, `RuleTyped`, …).
5. Two-phase accept (constructor names of the grammar front end change ⇒
   `GrammarParser.y` snapshot changes ⇒ `ASTAdapter.hs` must be updated to
   match the new constructor names in the same commit).
6. Docs: README grammar-format section, CHANGELOG, a unit test for the
   override and for the collision diagnostics.

### Acceptance

- grammar.pg parses itself with all alternatives named; `ASTAdapter` pattern
  matches read like prose (no `Ctr__Clause__14` left).
- Unnamed alternatives keep today's generated names byte-for-byte (goldens
  for all other corpus grammars unchanged unless they opt in).
- Fixed point + full battery green.

---

## Task 8b — Compile GrammarQQ into rtk (build prerequisite)

**Status: DONE.** Step 1 (the regex drop) landed with the asm.pg tutorial
work, which hit the regex scanner's newline bug for real; the rest landed
as its own change: `GrammarQQ` is compiled in the `generated-frontend`
component (template-haskell is its only extra dependency), `cabal test
unit` smoke-tests `[clause| … |]` quotes and `Anti_*` splices in both
contexts, and the `*QQ.hs` goldens are part of `make test-compile-goldens`.

### TL;DR

`GrammarQQ.hs` (generated, in `test/golden/grammar/`) is excluded from the
build because it imports `Text.Regex.Posix` and uses TH splices. Remove the
obstacle at the GENERATOR level — make generated quasi-quoters
dependency-light — then compile the snapshot's `GrammarQQ` into rtk next to
`GrammarLexer`/`GrammarParser`.

### Steps

1. In `GenQ.hs`, replace the `Text.Regex.Posix` metavariable scan
   (`qqPattern`, `str =~ qqPattern` in `replaceAllPatterns1`) with a
   hand-rolled scanner emitted into the generated module (plain `String`
   functions; the pattern `\$[A-Za-z_][A-Za-z_0-9]*[^A-Za-z_0-9:]` is easy to
   scan by hand). This drops `regex-posix`/`regex-base` from EVERY user's
   generated-QQ dependency footprint — document in README ("Using the
   generated code" section) and CHANGELOG. `make accept-golden` (all `*QQ.hs`
   goldens change uniformly); verify `make test-java-qq` still passes —
   it exercises `$var` rewriting, `$$` escapes and unknown-metavariable
   errors end to end.
2. Add `GrammarQQ` to the `generated-frontend` library component in
   `rtk.cabal` (Hs-Source-Dirs already points at `test/golden/grammar`);
   add `template-haskell` to that component's Build-Depends. Keep `-Werror`
   clean (the component already carries `-Wno-unused-matches` for alex
   output; TH-generated code may need another targeted `-Wno-*`, justify it
   in a comment like the existing one).
3. Smoke test inside the rtk test suite: a unit test that builds a clause
   with `[cl| Name '*' |]`-style quotes and pattern-matches one (proves
   compile-time parse + `Anti_*` splices work in-tree). Note the QQ produces
   the GENERATED AST types, not `IClause` — that mismatch is the point of 8c.
4. Watch the bootstrap loop: `GrammarQQ` is compiled from the snapshot, so
   QQ-generator changes also need the two-phase accept.

### Acceptance

- `import GrammarQQ` works in the rtk library/tests; a QQ-based unit test
  runs in `cabal test unit`.
- Generated QQ modules no longer require `regex-posix`/`regex-base`
  (README dependency list shrinks).
- Fixed point + full battery green.

---

## Task 8c — Retire `InitialGrammar`: the pipeline consumes the generated AST

### TL;DR

The structural migration. Today: `GP.Grammar → ASTAdapter → InitialGrammar →
StringLiterals/Normalize → NormalGrammar → generators`. Target: the front
half of the pipeline (`StringLiterals.normalizeStringLiterals`,
`Normalize.normalizeTopLevelClauses`) consumes the generated AST directly;
`InitialGrammar`/`IClause`/`IRule` disappear from `Syntax.hs`; the
adapter shrinks to the token-level cleanup it still owes (delimiter
stripping, `unBackQuote`) or vanishes into the first normalization stage.
`NormalGrammar` and everything after clause normalization stay as they are.

### The hard decision: what happens to the reference front end

`Parser.y` produces `InitialGrammar`. If `InitialGrammar` goes away, choose:

- (A) Retire the hand-written front end entirely. The oracle role passes to
  the golden corpus + the byte fixed point + the error-parity fixtures.
  Deletes `Lexer.x`/`Parser.y`/`TokenProcessing` usage from the pipeline,
  `--use-handwritten`, and the dual harness plumbing in
  `test/TestSupport.hs` / `GoldenTests.hs` / `UnitTests.hs`. Biggest
  simplification, biggest loss of redundancy. Requires explicit maintainer
  sign-off — it deletes the safety net that caught every adapter bug so far.
- (B) Keep the reference but make it produce the generated AST types
  (hand-written happy actions constructing `Ctr__*` values). Keeps the
  harness, couples the hand-written parser to generated type names (8a makes
  those stable, which is why 8a comes first).
- (C) Keep `InitialGrammar` as a reference-only type plus a
  `InitialGrammar → GP.Grammar` converter used only by the harness.
  Least invasive, but maintains THREE representations; probably the worst
  end state, acceptable as a migration waypoint.

Default recommendation: (B) during the migration, then (A) as a separate
later decision once the migrated pipeline has soaked.

### Steps (assuming B)

1. Inventory every consumer of `IClause`/`IRule`/`InitialGrammar`
   (`StringLiterals`, the front half of `Normalize`, `Debug` printers and
   `--debug-rule` tracers, `PrintGrammar`, tests). The back half
   (`SyntaxRule*`/`LexicalRule`/`NormalGrammar`) is untouched.
2. Port `StringLiterals` and the pre-`NormalGrammar` part of `Normalize` to
   the generated AST (with 8a names this is mechanical; SYB still works —
   the generated types derive `Data`). Positions improve for free: the
   generated AST has `RtkPos` on every node, not just rules, so
   normalization diagnostics can point at the offending CLAUSE, not the
   rule header — note it in CHANGELOG when it happens.
3. Convert `Parser.y` actions to build `GP.*` values; drop
   `ASTAdapter.convertGrammar`; the AST-equality harness now compares
   `GP.Grammar` values directly (positions are equality-transparent
   `RtkPos`, so the position-included comparison needs a explicit
   position-projecting check — today's suite compares `getIRulePos`;
   preserve equivalent coverage).
4. `Syntax.hs` keeps only the normalized-grammar half. Update module docs,
   BOOTSTRAP.md ("How it works" diagram in src/generated/README.md),
   CLAUDE.md core-modules list.
5. Bootstrap wrinkle, now structural: the pipeline's SOURCE is typed against
   its own generated output. A grammar.pg change that alters constructors
   breaks the in-tree build until the snapshot is re-accepted — document the
   two-phase workflow prominently in BOOTSTRAP.md ("Changing the grammar
   language" gains a step: fix the pipeline's pattern matches).

### Acceptance

- `IClause`/`IRule`/`InitialGrammar` gone (or reference-only per the chosen
  option); `grep -rn "IClause"` returns only history/docs.
- Equivalence harness still compares both front ends on the whole corpus
  (option B) or has been consciously retired with sign-off (option A).
- Fixed point + full battery green; `--debug-parse`, `--debug-rule`,
  `--debug-stage parse` output updated and tested.

---

## Task 8d — Rewrites as quasi-quoted patterns (the payoff)

### TL;DR

With 8a–8c in place, rtk's own transformations can be written the way rtk
promises its users: pattern matching and rewriting over grammar ASTs with
quasi-quotes. Convert selected normalization steps and add a user-facing
rewrite hook.

### Steps

1. Internal dogfooding: re-express 2–3 self-contained normalization steps as
   QQ-pattern rewrites over the generated AST, e.g. optional desugaring
   (`removeOpts`: `[cl| $c ? |] → [cl| ( | $c ) |]`-style), string-literal
   extraction, the group-lifting laws from the flip. Use SYB `everywhere`
   with QQ patterns as the match arms. Each conversion must be
   golden-neutral (artifacts byte-identical) — that's the refactoring
   guarantee, enforce it by NOT touching goldens in those commits.
2. Evaluate honestly: if a converted step reads worse than the plain
   pattern-match version, keep the plain version and say so here — the goal
   is leverage, not ideology. (Expected sweet spot: clause-shape rewrites.
   Expected poor fit: anything needing the name-supply/state in
   `Normalize`'s monad.)
3. User-facing: design a small rewrite API so users of any generated
   `<Name>QQ` can do the same to their own ASTs — e.g. export from generated
   QQ modules a `rewrite<Name> :: (Data a) => [GenericQ (Maybe a)] → a → a`
   helper or simply document the `everywhere`/`extT` + QQ-pattern recipe
   with a worked Java example in `docs/` (extend
   `docs/java-quasi-quotation-tests.md`, add a `make` test).
4. CHANGELOG: this is the "rewrite toolkit" finally meaning it; consider it
   the headline of its release.

### Acceptance

- At least two normalization steps run as QQ rewrites with byte-identical
  goldens; a documented, tested recipe (or API) exists for user grammars.
- Fixed point + full battery green.

---

## Suggested sequencing

```
8a (named ctors)  ──┐
                    ├──▶ 8c (pipeline on generated AST) ──▶ 8d (QQ rewrites)
8b (QQ in build)  ──┘
```

8a and 8b can run in parallel sessions. 8c is the long pole and carries the
reference-front-end decision (get maintainer input before starting it). 8d
is incremental and can land step by step.
