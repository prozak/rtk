# Follow-up tasks: Let's Build a Compiler on RTK

Status: PLANNED (follow-up to milestone 0, PR #125).

Milestone 0 landed the parts 2-4 expression language (`calc.pg`) and the
QQ-pattern interpreter/feature checklist (`TestQQ.hs`). This file breaks the
rest of the roadmap into self-contained task blobs, each written to be pasted
into a fresh session as the task description. They are ordered by dependency:
task 1 is the language, 2 gives it executable semantics and a test harness,
3 is the compiler back end, 4 is the rewrite payoff, 5 is optional growth,
6 is the closing documentation pass, and 7 is the teaching text written in
the original's installment style.

Conventions and references every task should know about:

- The tutorial: Jack Crenshaw, "Let's Build a Compiler",
  https://compilers.iecc.com/crenshaw/ (16 parts). The language built here is
  TINY (parts 10-12), grown toward KISS (parts 13-14).
- The sibling project `../c-compiler/` is the convention reference: directory
  layout, Makefile shape, `run_tests.sh` harness, and the "emit assembly by
  building an AST of an assembly *grammar* via QQ splices, never strings"
  approach (`asm.pg`, `Codegen.hs`, `Emit.hs`).
- The local `README.md` ("RTK idioms this tutorial leans on") documents the
  load-bearing grammar idioms: precedence cascades with `,`-lifted
  pass-throughs, quoter-vs-shortcut naming, the one-anti-quote-shape-per-type
  rule (rule ORDER in the .pg file selects scalar vs list), and leaf
  destructors for token payloads.

Invariants every task must preserve:

- `make -C tutorials/lets-build-a-compiler test` green, and the tutorial
  stays self-contained: it reaches into the parent checkout only via
  `cabal exec` (so it remains extractable to its own repository).
- Every AST traversal in tutorial code is written with quasi-quotation
  patterns and antiquote splices; generated `Ctr__*` constructors appear
  only in leaf accessors/constructors (the `litVal`/`mkImm` pattern,
  positioned with `rtkNoPos`).
- The repo battery is untouched: tutorial grammars live here, not in
  `test-grammars/`, so `make test` (unit + golden) must not change.
- The README status/roadmap table is updated as part of each task.

---

## Task 1 — tiny.pg: the TINY language (parts 5-12)

### TL;DR

Grow the milestone-0 grammar into Crenshaw's TINY: program frame, variable
declarations, control flow, I/O statements, and boolean/relational operator
layers above the arithmetic cascade. This is the tutorial's headline claim —
the seven scanner/parser chapters (2-3, 6-7, 11-12) as one declarative file —
so the grammar should read like the tutorial's BNF.

### Design decisions (make them early, in the session)

- Replace `calc.pg` with `tiny.pg` (recommended: calc was the milestone-0
  verification spike and is a strict subset — `print` becomes `WRITE`;
  port the TestQQ checks), or keep both grammars side by side.
- Surface syntax fidelity: uppercase keywords (`IF`/`ENDIF`/`WHILE`/
  `ENDWHILE`/`READ`/`WRITE`/`PROGRAM`/`VAR`/`BEGIN`/`END`), `=` both as
  assignment (statement level) and equality (relation level) as in the
  tutorial — LALR separates them by context. Optional semicolons (part 12)
  are a fidelity decision: pick "none" (TINY 1.2 style) or `';'?` and
  document the choice.
- Relation rule shape: `Relation = AddExpr RelOp AddExpr | ,AddExpr` (two
  explicit alternatives) is cleaner for QQ patterns than an optional-suffix
  proxy; check `--analyze-conflicts` either way.

### Sketch

    Program  = 'PROGRAM' Var DeclList 'BEGIN' StmtList 'END' '.' ;
    DeclList = Decl* ;
    Decl     = 'VAR' Var | 'VAR' Var '=' Num ;
    StmtList = Stmt* ;                       -- BEFORE Stmt: list anti-shape
    Stmt     = Var '=' Expr
             | 'IF' Expr StmtList 'ENDIF'
             | 'IF' Expr StmtList 'ELSE' StmtList 'ENDIF'
             | 'WHILE' Expr StmtList 'ENDWHILE'
             | 'READ' '(' VarList ')'
             | 'WRITE' '(' ExprList ')' ;
    -- boolean cascade: Expr = '|' level -> '&' level -> '!' level ->
    -- Relation -> AddExpr -> Term -> Factor, all sharing type Expr via
    -- ,-lifted pass-throughs; ExprList AFTER the cascade (scalar anti-shape)

Mind the two rule-order constraints (see README): `StmtList` before `Stmt`,
the `Expr` cascade before `ExprList`/`VarList`-style lists of expressions.

### Acceptance

- `make test` here: the existing QQ checklist ported, plus new pattern
  checks for control flow — `[stmt| IF $e1 $body ENDIF |]` binds an `Expr`
  and a `[Stmt]`; both IF forms distinguishable; `WHILE` round-trips through
  construction splices.
- A handful of `tests/valid/*.t` TINY programs parse (a `--parse` driver
  mode or a TestQQ case); `tests/invalid/*.t` programs are rejected with a
  position-bearing parse error.
- Conflict count from `--analyze-conflicts` is zero or each conflict is
  explained in a grammar comment.

---

## Task 2 — The interpreter back end and the program test harness

### TL;DR

Crenshaw's part 4 at full scale: `Interp.hs` executes TINY programs (the
milestone-0 evaluator only handled expressions/assignments), a `Main.hs`
driver runs files, and a `run_tests.sh` harness (mirror `../c-compiler/`'s)
turns `tests/` into the tutorial's regression suite. The interpreter is what
makes every later task testable without an assembler.

### Steps

1. `Interp.hs`: environment (`Map Var Int`), statement execution incl. both
   IF forms and WHILE, `READ` consuming a list of ints, `WRITE` appending to
   an output list — every construct destructured with `[stmt| ... |]` /
   `[expr| ... |]` patterns. Runtime errors (unbound variable) carry the
   variable name.
2. `Main.hs`: `tiny --run prog.t` (input ints from stdin, output ints to
   stdout, one per line) and `tiny --parse prog.t` (AST dump, for grammar
   debugging). Exit codes per the c-compiler contract: 0 on success,
   non-zero on parse/runtime failure.
3. `tests/valid/<name>.t` + `<name>.input` (optional) + `<name>.expected`;
   `run_tests.sh` diffs actual vs expected output. Seed with programs that
   exercise the whole language: gcd, fibonacci, nested IF/WHILE, READ/WRITE
   round-trip, declaration initializers.
4. Wire `./test-qq && ./run_tests.sh` under `make test` (same shape as the
   C tutorial's Makefile).

### Acceptance

- All sample programs produce their expected output; an
  intentionally-failing program demonstrates the runtime error path.
- `make -C tutorials/lets-build-a-compiler test` runs QQ checks + harness.

---

## Task 3 — 68000 code generation through an assembly grammar

### TL;DR

The compiler proper. Crenshaw emits Motorola 68000 assembly; do the same,
but the c-compiler way: define `m68k.pg` for the instruction subset the
tutorial emits, and make `Codegen.hs` a TINY-AST → asm-AST translation
built entirely from QQ construction splices (`[asmItems| ... |]`), with a
pretty-printer (`Emit.hs`) at the very edge. No string-concatenation
code generation anywhere.

### Fidelity contract

Mirror part 10's exact sequences so the README can show them side by side
with the tutorial's Pascal: `MOVE X(PC),D0` (load), `LEA X(PC),A0` +
`MOVE D0,(A0)` (store), push/pop through `-(SP)`/`(SP)+`, `ADD`/`SUB`
(with the `NEG` trick)/`MULS`/`DIVS`, relops via `CMP (SP)+,D0` + scc
(`SEQ`/`SNE`/`SLT`/...), branches `BEQ`/`BRA` with an `Lnn` label supply,
and the `READ`/`WRITE` library-call stubs. Where this project deviates
(e.g. no SK*DOS runtime), say so in the README rather than silently.

### Steps

1. `m68k.pg`: labels, the instruction subset, operand sorts (register,
   immediate `#n`, PC-relative symbol, pre/post-decrement). Look at
   `../c-compiler/asm.pg` for the grammar shape and at its `Emit.hs` for
   the printing contract.
2. `Codegen.hs`: symbol table pass over declarations first — duplicate and
   undeclared variables are *compile-time* diagnostics (Crenshaw part 10's
   error behavior), then statement/expression translation with a label
   supply (State monad). Leaf bridges (`mkImm`, `mkLabel`, ...) with
   `rtkNoPos`, as in the C tutorial.
3. `Main.hs` grows `tiny --compile prog.t -o prog.s`.
4. Tests: golden `.s` files for the sample programs from task 2, text-diffed
   by `run_tests.sh` (no assembler dependency in CI); the interpreter
   remains the semantic oracle for the same programs.

### Acceptance

- Every `tests/valid` program has a `.s` golden produced via the asm AST;
  regenerating is byte-stable; the README shows one tutorial-vs-generated
  assembly excerpt.
- Symbol-table diagnostics covered by `tests/invalid` cases (duplicate VAR,
  use of undeclared variable).

---

## Task 4 — Rewrites: FOR/REPEAT desugaring and constant folding

### TL;DR

The payoff chapter — what the single-pass original structurally cannot do.
Add part 5's extra control constructs (`REPEAT ... UNTIL e`, `FOR v = e1 TO
e2 ... ENDFOR`) to tiny.pg as *surface sugar only*: a `Desugar.hs` pass
rewrites them into core WHILE with QQ patterns + construction splices, so
the interpreter and code generator never learn about them. Add a constant
folding pass over `Expr` the same way. This is RTK's "rewrite toolkit"
pitch made concrete on a famous tutorial.

### Steps

1. Grammar: add the two statement forms (mind: new alternatives only — the
   anti-quote shapes and rule order from task 1 must not change).
2. `Desugar.hs`: SYB `everywhere` with QQ-pattern match arms, e.g.
   `[stmt| REPEAT $body UNTIL $e1 |]` → a WHILE on the negated condition
   with the body prepended (loop runs at least once). FOR needs a worked
   semantics decision: Crenshaw evaluates the limit each iteration in the
   simple scheme — re-evaluation is the honest, temp-free rewrite; document
   it. If a fresh temp variable is unavoidable, generate a reserved-name
   temp and reject user variables with that prefix at declaration time.
3. `Fold.hs` (or a section of Desugar): bottom-up `everywhere` over `Expr`;
   arms like `[expr| $e1 + $e2 |]` with both sides literal → `mkNum`;
   include the unary-minus and mul/div identities worth showing
   (`$e1 * 1`, `0 + $e1`). Synthesized literals via the leaf constructor
   with `rtkNoPos`.
4. Pipeline: `parse → desugar → (fold?) → interp/codegen`; `--no-fold` flag
   so the golden `.s` diff with/without folding can be shown in the README.
5. Tests: REPEAT/FOR programs through BOTH back ends; structural QQ checks
   (desugared tree equals the hand-written WHILE quote — position-
   transparent equality makes this a one-liner); folding goldens.

### Acceptance

- Core passes (interp, codegen) contain no REPEAT/FOR cases.
- A README section walks one desugaring rule and one folding rule, quoting
  the actual code.

---

## Task 5 (stretch) — KISS subset: procedures and types (parts 13-14)

### TL;DR

Where the tutorial outgrows TINY. `PROCEDURE name(params) ... END` with
by-value parameters and a call statement (part 13); `BYTE/WORD/LONG`
declarations with widening conversions (part 14). Roughly doubles the
grammar and the code generator; gate it on tasks 1-4 having soaked, and
split the two parts into separate sessions if taken. Scope honestly: stack
frames, parameter passing and type-directed instruction selection
(`MOVE.B/.W/.L`) are each a real chunk of work; the interpreter keeps both
features cheap to verify before touching codegen.

### Acceptance (sketch)

- Recursive gcd via PROCEDURE runs in the interpreter and compiles;
  mixed-size arithmetic emits the tutorial's conversion sequences.

---

## Task 6 — The chapter-by-chapter mapping document (closing pass)

### TL;DR

Turn the README's thesis into the flagship write-up: a part-by-part table
(Crenshaw part → its RTK counterpart → pointer into this directory), an
honest line-count comparison against the original TINY 1.3 Pascal listing,
the single-pass-vs-AST architecture discussion, and the "what the original
does better" section (Crenshaw's `Expected X` error UX vs generated
line/column errors; case-insensitive keywords; compile-as-you-parse memory
footprint). Quote real code for the showcase moments: the seven scanner
chapters next to the final tiny.pg, and one QQ rewrite rule.

### Acceptance

- The README (or a linked `MAPPING.md` if it outgrows the README) covers
  all 16 parts, including the ones deliberately not implemented (15-16,
  unit construction — moot under cabal) with one-line reasons.
- `tutorials/README.md`'s entry updated to its final wording.
- Cross-links with task 7: the mapping is the reference for readers who
  know the original; the installments are the path for readers who don't.

---

## Task 7 — The RTK installments: a teaching page in the original's spirit

### TL;DR

Crenshaw's tutorial endures because of its form, not just its content:
short, conversational installments, each one starting from code that runs
and ending with code that runs, growing the compiler one concept at a time
("a very long time between drinks" is exactly what he avoids). Write that
text for RTK — "Let's Build a Compiler, with RTK" — teaching a newcomer to
build the TINY compiler from an empty directory, where each increment is a
few grammar rules and a few QQ clauses instead of a Pascal procedure. This
is NOT task 6: the mapping doc serves readers who know the original and
want the correspondence; this serves readers who don't and want to learn.

### Hard constraints

- Mimic the structure and pedagogy, never the prose. The original text is
  copyrighted (freely distributable, not freely adaptable): each
  installment links to the part of the original it parallels, quotes
  nothing beyond a short attributed phrase, and is written from scratch.
- Every installment ends runnable, and no snippet may rot: installment N
  gets a checkpoint directory `steps/NN-<name>/` (the grammar and driver
  exactly as of that installment) that `make test` builds and runs in CI,
  and the page's code blocks are kept verbatim-identical to checkpoint
  files. A grep-level sync check in the test harness is enough — this is
  a discipline, not a literate-programming toolchain.
- Each installment stays short (Crenshaw-length: one sitting) and ends
  with "run this; you should see exactly this output".

### Installment ladder (sketch — refine in session)

0. The cradle (his part 1): toolchain check, a one-rule grammar, a tour of
   the three generated files.
1. Expressions (parts 2-3): single digits first — his famous opening — then
   the precedence cascade; run the parser on real input.
2. Seeing the trees (part 4): QQ patterns, a five-line evaluator; the
   moment the AST pays off.
3. Statements (interlude): assignment and WRITE; lists and the whole-body
   binder ($body).
4. Control constructs (part 5): IF/ELSE/WHILE as three grammar lines plus
   three interpreter clauses.
5. Boolean expressions (part 6): growing the cascade upward; relations.
6. TINY, and where did the scanner go? (parts 7, 10-12): the full language;
   the punchline that his two scanning chapters are five lexical rules.
7. Code generation (part 10): the m68k grammar; emitting an AST, not
   strings; side-by-side with the original's output.
8. Things the original couldn't do (parts 5+12 revisited): REPEAT/FOR as
   sugar, desugaring and constant folding as QQ rewrites.
9. Closing: KISS and beyond (task 5), and the mapping table (task 6) for
   readers coming from the original.

### Steps

1. Decide single page vs a `tutorial/` directory with one file per
   installment plus an index (recommended: the directory; the README stays
   the project front page and links to it).
2. Write installments 0-3 from the task-1/2 code base, building the
   `steps/` checkpoints as you go and wiring them into `make test`.
3. Installments 7-8 follow tasks 3-4; grow the page alongside those
   sessions rather than after them, so the teaching angle can push back on
   API/ergonomics decisions while they are still cheap to change.
4. Link the page from `tutorials/README.md` and the project README as the
   recommended entry point for learning RTK.

### Acceptance

- A newcomer with only the repo checkout can follow installment 0 through
  the end, reproducing every shown output.
- All `steps/` checkpoints build and run in CI; the block-vs-checkpoint
  sync check passes.
- No reproduced prose from the original; every installment cites the part
  it parallels.

---

## Suggested sequencing

```
1 (tiny.pg) ──▶ 2 (interpreter + harness) ──▶ 3 (68k codegen via m68k.pg)
                       │                              │
                       ▼                              ▼
        7 (teaching installments 0-6) ──▶ 7 (installments 7-8) ◀── 4 (rewrites)
                                                      │                │
                                                      ▼                ▼
                                            6 (mapping doc, last)   5 (KISS, optional)
```

Tasks 1-2 are one natural session each; 3 and 4 are the meaty ones. 7
starts once 1-2 exist and grows alongside 3-4 (one session per 2-3
installments is a good cut). 6 lands last so it documents what exists.
