# Plan: PL/0 tutorial follow-ups — from validator to full pl0c-parity compiler

Status: PLANNED (follow-up to the parts 1–3 validator, PR #126).

The tutorial currently covers parts 1–3 of Brian Callahan's
["Let's write a compiler"](https://briancallahan.net/blog/20210814.html)
series: `pl0.pg` generates the front end, `Main.hs` is the validator, and
`TestQQ.hs` proves the full QQ feature set on the generated AST (its `cg*`
functions are a miniature of the real code generator). The reference
implementation for language behavior beyond the blog posts is Callahan's
[`pl0c`](https://github.com/ibara/pl0c) (the blog's compiler, final state).

This file sketches the remaining work as task blobs. Each blob is written to
be pasted into a fresh session as the task description. Order: task 1 and 2
make the compiler real (1 before 2, matching pl0c, whose checks predate its
code generator), 3–5 follow the series' extension parts one post at a time,
6 is the RTK-flavored payoff the C original cannot express. Task 7 is core
RTK work surfaced by this tutorial (and by c-compiler), independent of 1–6.
Task 8 is the documentation track — a walkthrough page mirroring the
original series — startable now for parts 1–3 and growing a section as
each feature task lands.

Invariants every task must preserve:

- Tutorial self-containment: only reach the parent checkout through
  `cabal exec` (see `tutorials/README.md`); no files under `test-grammars/`
  or `test/golden/` (task 7, which is core-RTK work, is the exception).
- `make -C tutorials/pl0-compiler test` green, and stays wired in CI under
  that exact invocation.
- `run_tests.sh` keeps the contract: everything under `tests/valid` is
  accepted end to end, everything under `tests/invalid` is rejected with a
  diagnostic that names a line.
- Wirth/pl0c fidelity: language changes follow the series (and pl0c where
  the posts are silent); deviations get a README note like the existing
  sign-in-continuation one.
- `README.md` (layout table, status line, roadmap) updated in the same
  commit as the feature it describes.
- Once `TUTORIAL.md` exists (task 8): a task that changes the language,
  the pipeline, or the test harness updates the corresponding walkthrough
  section in the same commit — its code snippets are copied from the
  working files and must stay current.

---

## Task 1 — Semantic analysis (`Check.hs`): pl0c's check set with positioned diagnostics

### TL;DR

The validator accepts programs that crash any real PL/0 compiler:
`call x` where `x` is a variable, assignment to a constant, undeclared
identifiers, procedures nested two deep. Add a `Check.hs` pass between
parsing and (future) code generation implementing pl0c's semantic rules,
with diagnostics that carry real line numbers via the `RtkPos` fields the
generated AST already stores (`rtkPosOf`, `instance RtkPosOf <Type>` in
`gen/Pl0Parser.hs`).

### Reference behavior (pl0c)

- Symbols are `const` / `var` / `procedure`, declared at depth 0 (program
  block) or depth 1 (inside a procedure); a `procedure` inside a procedure
  is an error ("nesting depth exceeded").
- CHECK_LHS: an assignment target must be a `var` (not const, not proc).
- CHECK_RHS: expressions may reference consts and vars, never procedures.
- CHECK_CALL: `call` targets must be procedures.
- Undeclared identifiers are errors everywhere; duplicate declarations in
  the same scope are errors; procedure-local symbols disappear at the end
  of the procedure (pl0c's `destroysymbols`).

### Current mechanics

- `Main.hs` does `scanTokens content >>= parsePl0` and pretty-prints; the
  `Either String` pipeline and `renderError` (for `LINE:COL:message`) are
  the model for threading check failures.
- QQ patterns (`[statement| $id1 := $e1 |]`, `[statement| call $id1 |]`,
  `[condition| ... |]`) keep the traversal readable — see `TestQQ.hs` for
  the idiom; `cgIdent`-style leaf unwrapping gives the name, `rtkPosOf`
  gives the position.

### Steps

1. `Check.hs`: `checkProgram :: Program -> Either String ()` (error string
   in the same `LINE:COL:message` encoding the lexer/parser use, so
   `renderError` and `run_tests.sh`'s "names a line" check work unchanged).
   Scope environment as `Data.Map String Kind` plus depth; walk
   Block/Statement/Condition/Expression with QQ patterns.
2. Wire into `Main.hs` between parse and print (a `--no-check` escape hatch
   is not needed; the tutorial narrative is parse → check → emit).
3. New corpus under `tests/invalid/`: `call-a-var.pl0`,
   `assign-to-const.pl0`, `undeclared.pl0`, `duplicate-decl.pl0`,
   `nested-procedure.pl0`, `proc-in-expression.pl0`. Valid corpus gains a
   program with procedure-local shadowing of a global (legal; pins the
   scoping rules).
4. README: status line moves to "parts 1–3 + semantic checks"; layout table
   gains `Check.hs`.

### Acceptance

- All new invalid programs rejected with line-numbered messages; existing
  13 QQ tests and valid corpus untouched and green.
- `primes.pl0` (procedure-local `var n`) still passes — locals must not
  leak out of `isprime`.

---

## Task 2 — C code generator (series part 5): `pl0 file.pl0` produces a runnable binary

### TL;DR

Promote `TestQQ.hs`'s miniature `cg*` functions into a real `Codegen.hs`
that emits C the way pl0c does, and grow `Main.hs` from validator into
compiler driver: `pl0 file.pl0` writes C, compiles it with `cc`, and leaves
an executable next to the source (the same contract as the c-compiler
tutorial's `ncc`). `run_tests.sh` then runs every valid program.

### Reference behavior (pl0c)

C shapes: `static long x;` for vars, `static const long c = N;` for
consts, `static void name(void) { ... }` per procedure (forward-declared),
`int main(void)` for the program block, operators mapped `= → ==`,
`# → !=`, `:= → =`, `odd e → (e % 2 != 0)` wrapped in `if`/`while`
parentheses. Consult pl0c.c for exact spellings where it matters; byte
compatibility with pl0c's output is NOT a goal, behavior is.

### Current mechanics

- `TestQQ.hs` already contains the QQ-pattern codegen skeleton for
  statements/conditions/expressions (keep the test copy small and
  independent; `Codegen.hs` is the real one and may share nothing).
- The Block shell (`Ctr__Block__0 _ constOpt varOpt procs stmtOpt`) and the
  optional-declaration proxies are plain-constructor territory — one
  function each, like `cgStmtOpt`.
- The c-compiler tutorial (`../c-compiler/Main.hs`, `run_tests.sh`) is the
  model for the driver contract and the gcc handoff.

### Steps

1. `Codegen.hs`: `codegen :: Program -> String` (C text). Declarations
   first (consts, vars, procedure prototypes), then procedure bodies, then
   `main`. Procedures see globals; statement/expression cases via QQ
   patterns as in `TestQQ.hs`.
2. `Main.hs`: parse → check (task 1) → emit `<base>.c` → `cc` →
   executable next to the source; keep `--lex-only` and add `--emit-c`
   (stop after writing C; used by tests and debugging).
3. `run_tests.sh`: valid programs now must compile AND run (exit 0); keep
   the parse-reject loop for invalid ones. Since baseline PL/0 has no I/O,
   running checks termination only — output assertions arrive with task 3.
4. README: status "parts 1–5"; layout table gains `Codegen.hs`; roadmap
   trimmed.

### Acceptance

- Every `tests/valid/*.pl0` compiles to a binary that exits 0 (including
  `minimal.pl0` — an empty `main`).
- A deliberately slow-but-terminating program (e.g. `primes.pl0` with
  `max = 100`) runs in well under a second — no codegen-induced loops.
- `tests/invalid` behavior unchanged; no `.c`/binary artifacts left behind
  for rejected programs (mirror c-compiler's "no artifacts on failure"
  check in `run_tests.sh`).

### Design decision (make it early, in the session)

Emit C as text (pl0c-faithful, simplest) or through a small C-output
grammar with QQ splices, the way c-compiler emits assembly via `asm.pg`?
Default to text for this task — the asm-grammar approach is a possible
later refactor once the emitted subset stabilizes; note it in the README
roadmap instead of doing both at once.

---

## Task 3 — I/O statements (series part 6): programs that print

### TL;DR

Add the series' I/O statements — `writeInt e`, `writeChar e`,
`readInt [into] x`, `readChar [into] x` (spellings per the blog post /
pl0c; verify there, including whether `into` is optional) — as Statement
alternatives in `pl0.pg`, with codegen to `printf`/`getchar`-family C and
semantic checks (targets of `read*` must be vars). First task where sample
programs produce observable output.

### Steps

1. `pl0.pg`: new Statement alternatives (keywords come free from string
   literals). Keep the grammar conflict-free; document any new conflict
   like java.pg does if one is unavoidable.
2. `Check.hs`: `read*` targets are CHECK_LHS-style (var only); `write*`
   expressions are CHECK_RHS.
3. `Codegen.hs`: `writeInt` → `printf("%ld\n", ...)`-equivalent (match
   pl0c's exact output format — the test expectations depend on it),
   `readInt` via the same approach pl0c uses.
4. Samples: `square.pl0` gains `writeInt squ` (output 1,4,…,100 — restore
   the tutorial's classic shape with `call square` inside the loop);
   `primes.pl0` prints the count. New golden outputs: `run_tests.sh`
   compares program stdout against a sibling `.expected` file when one
   exists, exit code otherwise.
5. `TestQQ.hs`: one construction + one pattern test per new statement form
   (the QQ suite is the regression net for grammar changes).

### Acceptance

- `square.pl0` and `primes.pl0` produce their `.expected` output through
  the full pipeline; QQ suite green with the new cases.

---

## Task 4 — Arrays (series part 7)

### TL;DR

Part 7 adds arrays: declaration via `size` in the `var` section and
`x[e]` indexing in factors and assignment targets (exact syntax per the
post/pl0c). Grammar deltas in `pl0.pg`, an array/scalar distinction in the
symbol table (pl0c tracks a size per symbol and checks "is an array" /
"is not an array" at use sites), bounds-free C indexing in codegen, plus
corpus: valid array programs and invalid ones (indexing a scalar,
`size 0`, assigning to a whole array).

Keep an eye on the Factor rule: `Ident` vs `Ident '[' Expression ']'` is
an LALR-friendly pair, but verify the grammar stays conflict-free; if not,
document the conflict and its resolution in pl0.pg comments.

---

## Task 5 — Strings, `forward`, `exit`, logical operators, `mod` (series part 8)

### TL;DR

The last extension post brings pl0c to its final language: string
literals for `writeStr`, `forward` procedure declarations (mutual
recursion — the symbol table gains a forward kind that a later
`procedure` definition satisfies; an unsatisfied `forward` is an error),
`exit e`, `and`/`or`/`not` in conditions, and `mod`/`%`. Follow pl0c for
the exact token set (it accepts both keyword and symbol spellings for
some operators). After this task the language is feature-complete with
respect to the series; a `rot13.pl0`-style sample exercises strings,
arrays and I/O together.

The lexer grows a string-literal rule with escapes — mind
`docs/why-qq-limitations.md` (`$$` escape) since `$` inside PL/0 strings
would otherwise collide with antiquote rewriting in QQ tests.

---

## Task 6 — An optimizer pass (`-O`): rewrite rules the C original can't express

### TL;DR

Promote `TestQQ.hs`'s `simplify`/`optimize` demo into a real `Optimize.hs`
pass behind a `-O` flag: identity-element elimination (already
demonstrated), constant folding (`2 + 3 → 5`: match
`[expression| $n1 + $n2 |]`, compute over the `Ctr__Number__0` payloads,
splice the result back as a fresh Number node), strength/branch trivia
(`if odd 1 then s → s`) — each rule a QQ pattern, lifted over the tree
with SYB's `everywhere`, iterated to a fixed point. This is the
tutorial's unique selling point versus the single-pass C original: pl0c
structurally cannot have this pass. Acceptance: `--emit-c` diffs show the
folded constants; semantics of every `tests/valid` program unchanged
(same `.expected` output with and without `-O`).

---

## Task 7 — (core RTK) Scalar and list antiquotes for the same type

### TL;DR

A type currently gets ONE antiquote shape — scalar or list, whichever its
grammar normalizes first (`_antiRuleCache` in `Normalize.hs` is keyed by
type name only; `addAntiRuleCached` returns the cached constructor and
silently drops the second shape's registration). Both tutorials design
around it: c-compiler orders `StatementList` first to get list binders and
forgoes scalar `$statement1`; pl0-compiler keeps scalar `$s1` and matches
`begin` blocks positionally because `$stmts` cannot bind the whole list.
Fix RTK so both shapes coexist, then collapse the workarounds.

### Sketch

1. Key the cache by `(typeName, isList)`; emit both `AntiRule`s. The
   parser side already produces per-context tokens (`$Type:x` scalar,
   `$ListRule:x` in list position); the generated-QQ side needs distinct
   handler names (`antiNameGen` derives names from `arQQName`, which is
   the type name for both shapes today — derive the list one from the
   list rule's name to avoid the collision).
2. Pin behavior in a corpus grammar (i14-style) with quoter tests for
   scalar AND list binders on one type; `make accept-golden` + review the
   churn (every grammar with lists regenerates).
3. Collapse the tutorial workarounds: c-compiler regains `$statement1`,
   pl0-compiler's `begin $stmts end` binds `[StatementOpt]`; update the
   "Known RTK limitations" section of `tutorials/c-compiler/README.md`
   and the cross-reference in `tutorials/pl0-compiler/README.md`, and
   `docs/why-qq-limitations.md`.

Out of scope here: mixed list patterns (`begin $stmts ; x := 1 end` as a
pattern) — that is a separate, harder feature; construction already
supports the mixed form.

---

## Task 8 — `TUTORIAL.md`: the original series, retold with RTK

### TL;DR

A walkthrough page that mimics Brian Callahan's series part by part — the
same journey (pick a language, build a lexer, build a parser, test it,
generate code, extend the language), but at every step the reader does it
with RTK instead of hand-written C. The audience is someone reading (or
having read) the original who wants to see what each part collapses into
when a grammar file generates the front end and quasi-quotation drives the
passes. The working files in this directory ARE the tutorial's code: every
snippet on the page is copied from `pl0.pg` / `Main.hs` / `TestQQ.hs` /
`tests/`, so the page cannot drift from reality without the invariant
above catching it.

### Ground rules

- Original prose only. Mirror the structure and the pedagogical beats, do
  not reproduce the original's text; open every section with a link to the
  corresponding post (`https://briancallahan.net/blog/2021MMDD.html`) and
  open the page with a clear "this follows, and assumes you may be
  reading, Callahan's series" attribution.
- Honest accounting: where the original's part is mostly *gone* (the
  ~370-line lexer.c becomes the six lexical lines at the bottom of
  `pl0.pg`), say so and show what replaced it; where RTK genuinely differs
  (multi-pass with an AST instead of single-pass emission; LALR instead of
  recursive descent — the dangling-else discussion becomes a conflict
  discussion), teach the difference instead of hiding it.

### Shape of the page (section ↔ original part)

1. *Planning* (part 1, 2021-08-14): same language choice (PL/0, same EBNF),
   different plan — grammar first, passes against the generated AST. State
   the multi-pass deviation up front.
2. *A lexer* (part 2, 2021-08-15): the lexical section of `pl0.pg` —
   `ident`, `Integer: number` (typed token payloads), `Ignore:` rules for
   whitespace and `{ ... }` comments, keywords appearing for free from the
   string literals in syntax rules. Show `pl0 --lex-only` and a positioned
   lexical error next to the original's hand-rolled equivalents.
3. *A parser* (part 3, 2021-08-16): EBNF → `.pg` translation as a
   teaching sequence — the `Expression`/`Term`/`Factor` chain with
   `,`-lifted pass-throughs (one AST type, transparent parens),
   `StatementOpt = Statement? ;` as Wirth's `[ statement ]` made literal,
   `@shortcuts`, separated lists (`+ ~ ','`). End where the original ends:
   a validator. Include the sign-in-continuation story as the fidelity
   payoff (`-a + +b` is invalid PL/0, and the grammar rejects it because
   the EBNF does).
4. *Testing* (part 4, 2021-08-17): `tests/valid` / `tests/invalid`,
   `run_tests.sh`'s positioned-diagnostic contract.
5. *A code generator* (part 5, 2021-08-18): until task 2 lands, this
   section walks `TestQQ.hs`'s miniature `cg*` functions — QQ patterns as
   the code generator's dispatch — and points at `PLAN.md`; task 2 then
   rewrites it around `Codegen.hs`.
6. *Extensions* (parts 6–8): stubs linking the original posts and the
   matching PLAN tasks, filled in by tasks 3–5 as they land.
7. *What the original can't do*: closes with the QQ rewrite-rule optimizer
   demo (today from `TestQQ.hs`; task 6 upgrades it to the real pass).

### Steps

1. Write `TUTORIAL.md` covering sections 1–5 against today's code (5 in
   its interim, TestQQ-based form), with the parts 6–8 stubs.
2. Link it from `README.md` (top, next to the status line) and from the
   tutorial's entry in `tutorials/README.md`.
3. Verify every snippet against the working files (copy, don't retype);
   where a snippet shows command output (errors, AST fragments, test-run
   transcripts), generate it by running the command.

### Acceptance

- A reader following only `TUTORIAL.md` and the original posts can go
  from an empty directory to the passing `make test` state of parts 1–3
  without reading any other file in this repository.
- Every quoted grammar/Haskell snippet is byte-identical to the working
  files; every quoted command transcript reproduces.
- No sentence of the original series is reproduced; each section links
  its source post.

---

## Horizon (not scheduled)

Callahan's sequel series writes a self-hosting PL/0 compiler in PL/0.
After task 5 the language is rich enough to attempt it; it would be a new
top-level milestone with its own plan, and RTK plays no new role in it
(the self-hosted compiler is plain PL/0 source this compiler builds) —
park it until someone wants the war story.
