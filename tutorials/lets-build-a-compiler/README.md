# Let's Build a Compiler — with RTK

An implementation of Jack Crenshaw's classic tutorial
["Let's Build a Compiler"](https://compilers.iecc.com/crenshaw/) (1988-1995)
where the hand-rolled recursive descent parser — the bulk of the tutorial's
code — is replaced by an RTK grammar, and the later passes work on the
generated AST with quasi-quotation pattern matching and antiquote splices.

Crenshaw's compiler is single-pass by design: it emits code *while* parsing
and never builds a syntax tree. This project deliberately departs from that:
the grammar produces an AST, and what the tutorial does in seven chapters of
scanner/parser mechanics (parts 2-3, 6-7, 11-12) collapses into one `.pg`
file. The freed-up AST is what enables the pieces the original can't do,
like source-to-source rewrites written in concrete syntax.

## Status: milestone 0 (tutorial parts 2-4)

| File | Role |
|------|------|
| `calc.pg` | The expression/assignment language of parts 2-3 as an RTK grammar: precedence cascade, unary minus, parentheses, `{ }` comments |
| `TestQQ.hs` | Part 4's interpreter in miniature, written entirely with QQ patterns, plus the full quasi-quotation feature checklist (construction, scalar/list splices, pattern matching, whole-list binders) |

```sh
cd ../.. && cabal build      # build the RTK toolchain first
cd tutorials/lets-build-a-compiler
make test
```

## Roadmap

| Milestone | Tutorial parts | Deliverable |
|-----------|----------------|-------------|
| 0 (this)  | 2-4 | calc grammar + QQ interpreter + idiom verification |
| 1 | 5-12 | `tiny.pg`: the complete TINY 1.3 language (PROGRAM/VAR/IF/WHILE/READ/WRITE, boolean and relational operators) |
| 2 | 10 | two backends: an interpreter for executable tests, and a 68000 emitter mirroring the tutorial's exact instruction sequences |
| 3 | 5, 12 | the RTK showcase: FOR/REPEAT as grammar sugar desugared to core WHILE via QQ rewrites, constant folding as an AST pass |
| 4 (maybe) | 13-14 | procedures and types (the KISS subset) |

## RTK idioms this tutorial leans on

- **Precedence cascades**: every level (`Expr`/`Term`/`Factor`) shares one
  AST type and chains down with a `,`-lifted pass-through, so there are no
  wrapper constructors and `[expr| $e1 + $e2 |]` pattern-matches any
  addition node regardless of nesting depth.
- **Quoter vs. shortcut naming**: the quasi-quoter is the lowercased *type*
  name (`[expr| ... |]`, `[stmt| ... |]`); `@shortcuts` declarations only
  control how `$`-variable prefixes resolve (`$e1` → `Expr`, `$body` →
  `StmtList`).
- **One anti-quote shape per type**: rule order decides whether a type's
  antiquotes are scalar or whole-list. `StmtList` is declared before `Stmt`
  (so `$body` binds a `[Stmt]`), while `ExprList` comes after the `Expr`
  cascade (so `$e1` stays a single expression and `print($e1, $e2)` is a
  fixed-arity pattern).
- **Leaf destructors**: token payloads can't be bound by antiquotes, so
  literals go through one hand-written accessor (`litVal`) over the
  generated constructor; everything else is QQ.
