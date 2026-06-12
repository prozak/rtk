# Follow-ups: the Lisp interpreter tutorial (tutorials/lisp-interpreter)

Status: PLANNED (follow-up to PR #124, the initial lis.py port).

The tutorial is complete and green at the lis.py level: generated reader,
QQ-pattern `eval`, QQ macro expander, 47-case test suite, REPL. This file
lists the follow-up work it motivates, in the task-blob style of
`qq-grammar-rewrites-plan.md`: each task is written to be pasted into a
fresh session as the task description. L1–L2 are RTK features the tutorial
surfaced (valuable independently of it); L3 adopts the named-constructor
feature (task 8a, since landed); L4–L8 grow the interpreter toward
Norvig's fuller `lispy.py`; L9–L11 are pedagogy/infrastructure.

Invariants every task must preserve:

- `make -C tutorials/lisp-interpreter test` green (47+ cases and the
  example programs), from `make clean`.
- The cabal suites and the rest of the make battery stay green; RTK-core
  tasks (L1, L2) additionally go through `make accept-golden` with the
  diff reviewed, and keep the bootstrap fixed point (see BOOTSTRAP.md).
- The tutorial README is part of the deliverable: its code listings are
  copied from the real implementation, and §6/§8's "honest footnotes"
  must stay honest (when a task closes a documented limitation, update
  the text that documents it).

---

## L1 — Segment antiquotes for list tails (RTK core)

The README's §6 names this "the RTK feature this tutorial most obviously
motivates". QQ patterns are fixed-shape: `[expr| (begin $e1 $e2) |]`
matches exactly two subforms, and there is no way to write
`[expr| (begin $body...) |]` binding `body :: [Expr]` to the rest. Every
variadic spot in the interpreter (procedure application, n-ary `let`,
`begin` bodies) falls back to pattern synonyms over raw constructors.

Scope the first iteration to a segment metavariable in *tail position* of
a repetition (`$name..` or `$name...` as the last element inside a list
context): the parsed marker splits the list once, so `dataToPatQ` can be
extended to emit `(p1 : p2 : rest)` without view patterns. Mechanics
touch the generated lexer (a `qq_seg_<Type>` token), Normalize (a
list-position anti-alternative carrying the marker), and GenQ (pattern
side: cons-pattern emission; expression side: splice a `[Type]` into the
tail). Segment-in-the-middle and multiple segments are explicitly out of
scope. Acceptance: the tutorial's `eval` application clause and general
`let` rewritten as QQ patterns; a corpus grammar pinning the feature in
the golden suite; README §4/§6 footnotes updated.

## L2 — Escape Alex set operators in GenX character classes (RTK core)

Alex rejects bare `+`, `<`, `?` inside `[...]` sets (verified against
alex 3.5.4.2: `[+]` is a parse error), and GenX passes them through
unescaped — scheme.pg works around it by matching those characters as
string-literal alternates. Extend the #95 backslash treatment to the set
operator characters (emit them hex- or backslash-escaped). Related trap
discovered while rebasing over #95: a user-written `\\xNN` in a `.pg`
class now silently emits `\\xNN` (a literal backslash plus the text
`xNN`) — wrong lexing with no error. Either preserve `\x` pairs the way
`\n \t \r \f \v` are preserved, or reject them with a positioned
diagnostic; silence is the only wrong answer. Acceptance: a corpus
grammar with an operator-heavy class; scheme.pg's `sym` rule simplified
back to a single class and its README §2 bullet updated; goldens
regenerated and byte-identical under both front ends.

## L3 — Adopt named constructors (task 8a landed; ready to do)

Task 8a shipped per-alternative constructor labels
(`Expr = Add: Expr '+' Term | Term ;` names the constructor
`Add RtkPos Expr Term`; see "Named constructors" in the root README).
Label the alternatives in scheme.pg (`Num: num`, `Flt: flt`, `Str: str`,
`TrueL: '#t'`, `FalseL: '#f'`, `Sym: sym`, `List: '(' Expr* ')'`) and
delete the hand-written pattern-synonym block in `Main.hs` — or reduce
it to position-erasing wrappers, since named constructors still carry
the leading `RtkPos` field and construction sites still need `rtkNoPos`.
The README's §2 "RTK names constructors positionally" passage becomes a
feature demonstration instead of a workaround explanation.

## L4 — Tail calls (lispy.py parity)

`eval` recurses through `IO`, so deep Scheme recursion grows the Haskell
stack. Port lispy.py's explicit trampoline: `eval` becomes a loop that
rebinds `(env, expr)` for tail positions — the `if` branches, the last
form of a body, and the closure-call case in `apply`. Acceptance: a
test like `(define loop (lambda (n) (if (= n 0) 0 (loop (- n 1)))))`
with `(loop 1000000)` returning `0` in constant space.

## L5 — `define-macro` (lispy.py parity)

User-defined macros at runtime: `(define-macro name (lambda (args...)
template))` registers a closure that `expand` applies to the
*unevaluated* operand forms, alongside the built-in QQ rewrite rules.
Requires threading a macro table through `expand` (it stops being pure —
or takes the table as an argument seeded before each top-level form).
The didactic point: built-in macros are compile-time-checked QQ rules,
user macros are runtime AST functions, and they compose. Acceptance:
lispy.py's own example — `(define-macro unless2 (lambda (c b) (list
(quote if) c (quote #f) b)))` style — exercised in the test suite.

## L6 — Quote sugar `'x`, then quasiquote/unquote

`'x` for `(quote x)` is one grammar alternative plus one `expand`/`eval`
case; README §9 already pitches the comparison against what the same
sugar costs in a hand-rolled tokenizer. The bigger flex is `` `x `` and
`,x`: in-language quasiquotation implemented in an interpreter that is
itself built on quasiquotation — the README writes itself. Acceptance:
`'(1 2 3)` ⇒ `(1 2 3)`; `` `(1 ,(+ 1 1) 3) `` ⇒ `(1 2 3)`; tests and a
README section.

## L7 — Hygienic expansion (close the `or-tmp` footnote)

`(or a b)` expands through a temporary that user code can capture by
naming a variable `or-tmp` — documented in §6 as a deliberate
simplification. Thread a gensym counter through `expand` (making it
`Expr -> State Int Expr` or passing the counter explicitly) and generate
fresh names. Small task; its real deliverable is updating §6's hygiene
footnote from "could be fixed" to "here is the fix and what it cost"
(the expander stops being pure one-liners — say so).

## L8 — Scientific notation and Norvig's quote test verbatim

The lexer has `Integer`/`Double` tokens but no exponent form, so
Norvig's very first lispytest case — `(quote (testing 1 (2.0)
-3.14e159))` — is currently adapted away. Add the exponent alternative
to `flt` (java.pg's `exponentPart` is the in-repo precedent) and include
his test verbatim. One-line grammar change; the point is corpus
fidelity.

## L9 — The Design B companion: special forms in the grammar

README §9 calls porting the interpreter to the P-style design
(`Expr = '(' 'if' Expr Expr Expr ')' | ...`) "a one-evening exercise" —
provide the answer key. Either a worked appendix in the README (grammar
fragment, the `quote` reflection problem, what `(if)` as a parse error
buys) or a sibling implementation `scheme-typed.pg` + `MainTyped.hs`
behind the same 47-case test list, whichever stays smaller. The
interesting deliverable is the side-by-side: which clauses of `eval`
disappear into the parser, and what `quote`/code-as-data costs in
exchange.

## L10 — Align the test layout with the other tutorials (optional)

c-compiler and pl0-compiler keep a `tests/valid` / `tests/invalid`
corpus driven by `run_tests.sh` with positioned-diagnostic checks; this
tutorial embeds its cases in `Main.hs` (`--test`) with two standalone
examples. The embedded suite is good pedagogy (the cases are part of the
tutorial text) — keep it — but add a small `tests/invalid` corpus
(unbalanced parens, stray `)`, bad token) asserting on the positioned
parse errors, so error-path coverage survives refactors. Purely
consistency; lowest priority.

## L11 — A standalone teaching page in the shape of the original essay

The existing README is a *comparison*: it walks Norvig's essay section by
section and shows what each part becomes under RTK, assuming the reader
has (or knows) the original. Write the other document: a self-contained
course page ("How to Write a (Lisp) Interpreter (in Haskell, with RTK)")
that *teaches* building the interpreter from an empty directory, in the
original essay's didactic structure — no Python required. Chapter flow
mirrors Norvig's: what an interpreter does (parse → eval, his two-stage
diagram); the Scheme subset and its forms table; Part 1, parsing — where
Norvig writes `tokenize`/`read_from_tokens`, the reader grows `scheme.pg`
incrementally (atoms and lists first, run `rtk`, inspect the generated
AST in ghci; then add strings, booleans, floats and regenerate);
environments; Part 2, eval — one QQ-pattern clause per section, each with
a runnable checkpoint (constants → `quote` → `if` → `define`/`set!` →
`lambda` → application); `schemestr` and the REPL; and a closing chapter
the original doesn't have — the macro expander, RTK's payoff — before the
same "how about the rest of Scheme?" sign-off.

Two design decisions to make early. (1) Checkpoint mechanics: prefer
extracting the page's fenced listings and checking them verbatim against
`scheme.pg`/`Main.hs` (a small script wired into `make test`,
mechanizing this plan's "honest listings" invariant) over checked-in
per-stage snapshot directories, which would be six copies of the code
waiting to drift; intermediate stages the final code doesn't contain
(e.g. the atoms-only grammar) are the exception and may live as
unchecked listings clearly marked as scaffolding. (2) Placement:
`tutorials/lisp-interpreter/TUTORIAL.md`, with the README keeping the
project-page role (layout, build, the comparison walkthrough) and
cross-linking it as "start here to build it yourself". Write original
prose throughout — the structure and pedagogy are inspired by and
attributed to Norvig's essay, the text is not copied. Acceptance: a
newcomer can produce the working interpreter following only the page;
the listing-fidelity check is green; both documents link each other.
