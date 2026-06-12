# Plan: Write You a Haskell tutorial — follow-up task blobs (W1–W6)

Status: PLANNED (follow-ups to the WYAH tutorial, PR #92).

The tutorial at `tutorials/write-you-a-haskell/` implements chapters 1–12 of
Stephen Diehl's "Write You a Haskell" as four RTK-frontend interpreters
(`lc`, `stlc`, `poly`, `proto`): every semantic pass — desugaring, type
checking, algorithm W, evaluation — pattern-matches concrete syntax through
the generated quasi-quoters. 110 checks run under
`make -C tutorials/write-you-a-haskell test` (wired into CI). The tutorial's
README records the grammar design rules and the RTK findings that came out
of building it.

This file sketches the follow-ups as six self-contained task blobs, written
to be pasted into a fresh session as the task description. W1 and W2 are RTK
features the tutorial exposed (W2 is the second half of issue #95); W3–W6
are tutorial-side. They are independent of each other except where noted;
W4 benefits from W1 but does not require it, and W6 reads best after W3
closes the chapter-12 gap (it can land before, marking that section as
"lite").

Invariants every task must preserve:

- Tutorial suite green: `make -C tutorials/write-you-a-haskell test`
  (110 checks today; tasks are expected to grow that number, never shrink
  it) and the REPLs still start: `make repl-<lang>`.
- Repo suites green: `cabal build --ghc-options=-Werror --enable-tests`
  (CI uses GHC 9.6.4 with mtl 2.3 — import `Control.Monad` explicitly in
  drivers), `cabal test` (incl. the dual-front-end equality harness),
  `make test-compile-goldens`, and the make battery CI runs.
- RTK-side changes that alter generator output go through
  `make accept-golden` with the diff reviewed; changes to the grammar front
  end itself are two-phase (accept, rebuild so the `generated-frontend`
  component recompiles, re-run, re-check the fixed point per BOOTSTRAP.md).
- The tutorial stays self-contained: it reaches into the parent checkout
  only for the toolchain via `cabal exec` (see `tutorials/README.md`).

---

## Task W1 — QQ ergonomics: token-payload antiquotes and per-type scalar+list splices

### TL;DR

Two RTK limitations force boilerplate in every tutorial driver. (1) Token
payloads are not antiquotable: the `Int` of `num` or the `String` of `id`
have no `Anti_*` constructor, so each driver carries explicitly
bidirectional pattern synonyms (`pattern LitI n <- Ctr__Expr__0 _ n where
LitI n = Ctr__Expr__0 rtkNoPos n`, plus `IdN`, `ConIdN`, …). (2) A type's
anti rule is cached either scalar or list (`Normalize.addAntiRuleCached`),
so a type used in both positions needs a single-constructor wrapper type —
`poly.pg`/`proto.pg` define `Param`/`PArg`/`Field`/`TyVar` purely for this.
Remove both limitations; delete the boilerplate they forced.

### Where things stand

- `Normalize.addQQLexRuleCached` / `addAntiRuleCached` (the scalar-XOR-list
  cache and the `$Type:name` token synthesis).
- `GenQ`: `nonListGen` / `listPatGen` / `listExpGen` anti functions and the
  `extQ` chain; quasi-quote pattern generation wildcards `RtkPos` already.
- Boilerplate to delete on success: the pattern-synonym blocks at the top of
  all four `tutorials/write-you-a-haskell/*-main.hs`, the wrapper-type rules
  and their header comments in `poly.pg`/`proto.pg`, and rule 4 of the
  README's "Grammar design rules" section.
- Issue #95's "related smaller findings" section mentions both limitations.

### The work

- Per-type scalar AND list anti rules: distinct constructors (e.g.
  `Anti_X` and `Anti_X_list`) registered independently, with both `extQ`
  handlers generated. The grammar's `ListElem_*` productions keep deciding
  which token parses where; only the cache collapse goes away.
- Token-payload antiquotes need a design decision early in the session: the
  field is a plain `Int`/`String`, so there is no place to park a
  metavariable at parse time. Options: (a) opt-in per-token wrapper data
  (`data NumTok = NumTok Int | Anti_NumTok String`) changing the AST field
  type — clean, breaking, gate it behind a grammar annotation; (b) sentinel
  payload plus a side table threaded through the QQ parse — non-breaking,
  fragile; (c) construction-only smart constructors (no pattern side).
  (a) is recommended; it also gives literals positions for free.
- Add a `test-grammars/` grammar exercising both features so the repo golden
  and unit suites cover them (tutorial grammars are not golden-snapshotted).

### Acceptance

- All four drivers compile with their pattern-synonym blocks deleted;
  `poly.pg`/`proto.pg` lose the wrapper types; 110+ checks green.
- New repo-side grammar covers: scalar+list splices of one type, and
  pattern + construction antiquotes of an `Int`-typed token.

---

## Task W2 — Layout: indented ProtoHaskell through the issue #95 token filter

### TL;DR

`proto.pg` uses explicit `{ ; }` blocks because RTK cannot lex the offside
rule. Half of issue #95 has landed (generated lexers return positioned
tokens). Finish the other half — a supported token-filter hook between the
generated lexer and parser plus a reusable offside module — and wire it into
the tutorial so the same grammar accepts indented programs unchanged.

### Where things stand

- Issue #95 (updated 2026-06-10) records the remaining scope: a
  `[PosToken] -> [PosToken]` hook threaded through both the plain parse path
  and the quasi-quoter path, and a virtual-brace insertion module keyed on
  configurable layout keywords.
- Generated parse functions and `GenQ.qqFunImplGen` both call
  `scanTokens`/`parse<Name>` directly today; there is no seam.
- `tutorials/write-you-a-haskell/proto.pg`'s header comment and the README's
  proto section both point at #95.

### The work

- GenY: emit `parse<Name>With :: ([PosToken] -> [PosToken]) -> ...`
  alongside the current API; GenQ threads an optional filter the same way.
- A support module (shipped where generated code can import it — follow the
  decision made for the QQ runtime imports) implementing the offside
  algorithm: insert virtual `{`, `;`, `}` from column positions after
  layout keywords; the keyword set is per-grammar configuration.
- Tutorial: proto's driver opts in with `of` as the layout keyword; add
  indented test programs asserting AST equality with their braced
  equivalents; REPL keeps requiring braces (single-line input).

### Acceptance

- An indented `case` program parses to the same AST as its braced spelling
  (asserted in the proto suite); all existing braced tests unchanged.
- Issue #95 closed with a comment linking the commits.

---

## Task W3 — Chapter 12 completed: pattern-match compilation and multi-equation functions

### TL;DR

The tutorial's chapter 12 is implemented "lite": nested case patterns are
matched directly at runtime (`matchPat` recursion) rather than compiled to
simple case trees, and function declarations take only variable parameters.
Implement the chapter's actual content: multi-equation declarations with
patterns, compilation of nested matches to trees of single-depth cases (the
mixture-rule algorithm), and exhaustiveness/overlap diagnostics derived from
the compiled tree.

### Where things stand

- `tutorials/write-you-a-haskell/proto-main.hs`: `matchPat` (direct nested
  matching), `desugarE`/`desugarDecl` (QQ rewrites — currying, `let rec` →
  `fix`), `inferPat`, and the renamer's `patVars` arity/duplicate checks.
- `proto.pg`: `Decl` takes `Params` (plain `Id` wrappers); `Pat`/`PatAtom`/
  `PArgs` already express nested constructor patterns inside `case`.
- The README's proto section names the lite cut explicitly.

### The work

- Grammar: allow `let f <patatoms> = e ;` declaration equations (parameters
  become `PArg`s) and consecutive equations for the same name; renamer
  checks equal arity across a group.
- Desugar a group of equations into one curried lambda over fresh variables
  whose body is a `case` on the tuple… proto has no tuples — use nested
  cases column by column (this is exactly the mixture rule; the
  intermediate trees stay in the generated AST, built with `[expr| case … |]`
  splices).
- Compile nested patterns inside every `case` to single-depth matches the
  same way; `matchPat` then shrinks to flat constructor dispatch.
- Emit a warning (or error — decide and document) listing uncovered
  constructor shapes per compiled case; runtime "non-exhaustive" errors
  should become unreachable for warned-free programs.

### Acceptance

- `let map0 f Nil = Nil; let map0 f (Cons x xs) = Cons (f x) (map0 f xs);`
  type-checks to `(a -> b) -> List a -> List b` and runs.
- A non-exhaustive definition produces the diagnostic naming the missing
  constructor; the existing nested-pattern tests still pass through the
  compiled path (assert `matchPat` no longer recurses into constructor
  arguments, e.g. by construction).

---

## Task W4 — A native backend: compile Poly to x86-64 through the asm grammar

### TL;DR

The tutorial text ends at chapter 12; chapters 13–28 (core, STG, codegen)
were never written. The c-compiler tutorial already demonstrates the
RTK-native way to emit code: build an AST of an assembly *grammar*
(`asm.pg`) via quasi-quote splices and pretty-print it. Give WYAH a real
backend the same way: compile desugared Poly core to x86-64, staged so each
milestone is a working compiler.

### Where things stand

- `tutorials/c-compiler/` has `asm.pg`, `Emit.hs`, `Codegen.hs` and a
  gcc-driven test harness — the model to follow (and the source to copy
  `asm.pg` from; tutorials stay self-contained, so copy rather than import,
  noting the provenance).
- `poly-main.hs` already produces a minimal core: after `desugarDecl`/
  `desugarE`, programs are single-parameter lambdas, `let`, `fix`, `if`,
  operators, literals, variables.

### The work

- Milestone 0: closed first-order programs — integer arithmetic, `==`,
  `if`, top-level `let` of non-function values; emit AT&T x86-64 via
  `[asm| … |]` construction, assemble/link with gcc, exit code is the
  result (the c-compiler test-harness contract).
- Milestone 1: functions and `fix` — closure-convert the core (free-variable
  analysis exists in spirit in `lc-main.hs`'s `freeVars`), heap-allocate
  closures with a tiny C runtime shim, calling convention documented in the
  README.
- Design decisions to make early: where the backend lives (a fifth driver
  `polyc-main.hs` in the tutorial vs a subdirectory), and how much of the
  type checker gates compilation (recommend: programs must infer before
  codegen).

### Acceptance

- Milestone 0: `let x = 2 + 3; if x == 5 then 42 else 0;` compiles to a
  binary exiting 42; harness mirrors `run_tests.sh`.
- Milestone 1: the flagship `let rec fib n = …; fib 10;` compiles to a
  binary printing/exiting 55; CI runs the backend tests with the tutorial.

---

## Task W5 — REPL upgrade: line editing, commands, and file loading

### TL;DR

All four REPLs are bare `getLine` loops: no history, no editing, no
`:type`, no way to load a file. Extract the shared loop into one module and
upgrade it.

### Where things stand

- Each `*-main.hs` has a near-identical `repl` function (prompt, `:q`,
  exception guard, per-language eval/print); poly/proto additionally thread
  session state. The scaffolding is AST-agnostic; only the
  parse/check/eval/print step differs.
- The drivers compile against rtk's package environment via `cabal exec`,
  which does NOT provide haskeline. This is the design decision to make
  early: add `haskeline` to rtk.cabal's library deps (pollutes the tool's
  footprint), give the tutorial its own minimal cabal project (departs from
  the cabal-exec convention — discuss with the maintainer), or implement
  history-less editing on raw terminal input (not worth it). Recommend the
  second, mirroring how a tutorial would be extracted to its own repo
  (tutorials/README.md names that as a goal).
- `:load` needs no new dependency and can land first: `runProgram`/`execDecl`
  already accept multi-declaration sources in poly/proto.

### Acceptance

- A shared `Repl.hs` in the tutorial used by all four drivers; arrow-key
  history and editing work; `:type e` prints the inferred scheme (stlc,
  poly, proto), `:load tests/fib.poly` extends the session, `:quit`/`:q`
  exit; piped-stdin behavior (used by the test suites) unchanged.

---

## Task W6 — The teaching page: "Write You a Haskell, with RTK"

### TL;DR

A narrative tutorial document that mirrors the original's chapter arc —
parsing, lambda calculus, type systems, evaluation, Hindley-Milner,
ProtoHaskell — but teaches the RTK way at every step: write a `.pg`
grammar, generate the front end, then write each semantic pass as
quasi-quote patterns over concrete syntax. The reader finishes able to
build their own language front end with RTK. All the material exists (the
four checked-in languages, the README's design rules); this task is the
prose, the per-section runnable checkpoints, and the exercises.

### Where things stand

- `tutorials/write-you-a-haskell/README.md` is a *map* (status table,
  per-language summaries, design rules, findings) — reference material,
  not a lesson. The grammars and drivers carry section comments that
  already explain the why at each step.
- The original tutorial (https://github.com/sdiehl/write-you-a-haskell)
  provides the chapter structure readers may arrive from.

### Constraints (decide nothing here — these are fixed)

- The original's prose is Stephen Diehl's copyrighted work. The page
  mirrors structure and topic order only, in entirely original wording;
  per chapter, LINK to the original section instead of quoting it. Do not
  copy or lightly rewrite its text or its code listings; our own
  checked-in code is the only source for excerpts. Check the original
  repository's license before borrowing anything beyond chapter titles.

### The work

- Format decision early: single `tutorials/write-you-a-haskell/TUTORIAL.md`
  with chapter anchors (recommended to start; split into `book/` pages only
  if it outgrows one file). Linked from the tutorial README and
  `tutorials/README.md`.
- Per-chapter sections, each ending in a runnable checkpoint the reader can
  execute (`make test-lc`, a REPL transcript to reproduce, …):
  ch. 3 — grammars replace parser combinators (walk `lc.pg` line by line:
  ladder, lifts, `@shortcuts`, why it is conflict-free); ch. 4 — semantics
  as QQ patterns (`eval`, capture-avoiding `subst`, SYB one-layer
  recursion); ch. 5–6 — a second nonterminal family, building types by
  splicing, one evaluator with two strategies; ch. 7 — desugaring as QQ
  rewrites and algorithm W over the generated AST; ch. 8–12 — ADTs,
  renamer, case (and the explicit-brace decision, pointing at issue #95 /
  task W2; mark pattern compilation per task W3 status).
- Interleave the RTK lessons the project produced where the reader first
  needs them (anti-token placement, wrapper types, pattern synonyms,
  position transparency) — migrating, not duplicating, the README's
  "design rules" section: the README keeps the reference list and links to
  the teaching page for the worked explanations.
- Exercises with checked answers, e.g.: add a `<` comparison operator to
  `lc` end-to-end (grammar line, eval case, test); add a `Pair` library and
  `swap` to `proto`; make `if` desugar to a `case` on a user-defined Bool.

### Acceptance

- `TUTORIAL.md` exists, linked from both READMEs; every code excerpt is
  verbatim from checked-in files (cite `file:line` ranges next to each
  excerpt so drift is detectable); every checkpoint command works against
  the tree it documents.
- A parity table maps original chapters to sections, marking what is
  intentionally different (no NanoParsec, explicit braces) and why.
- No prose or code from the original tutorial is reproduced.
---

## Smaller items (no blob; fix opportunistically or bundle)

- Task 8a's named alternative constructors (`Lit: num`, see grammar.pg)
  landed after the tutorial: adopting them in the four tutorial grammars
  would replace the positional `Ctr__Expr__N` names in the drivers' pattern
  synonyms and error messages with readable ones, independently of W1.
- The QQ start-wrapper rule adds one benign shift/reduce conflict when the
  start symbol is nullable (`poly.pg`, `proto.pg`: empty `DeclList`); a
  non-nullable wrapper synthesis would make `happy -i` clean for them.
- `regexplit`'s inline classes still overrun when a class body ends in a
  backslash (the reason for the shared `backslash = [\\]` macro convention
  in `grammar.pg`/`java.pg`); the regexplit rule could consume escape pairs
  atomically the way the str rule now does.
- `Ignore: comment = '--' .* [\n]` requires a trailing newline: a `--`
  comment on the last line of input without `\n` is a lexical error in all
  four tutorial grammars.
- `stlc`'s closure printing drops the binder's type annotation
  (`<<closure>> \x . x` for `\x : Int . x`): `VClosure` doesn't store it.
