# Let's Write a Compiler — with RTK

An implementation of Brian Callahan's
["Let's write a compiler"](https://briancallahan.net/blog/20210814.html)
tutorial series (PL/0, Wirth's teaching language) in which RTK replaces the
two largest hand-written components: the lexer (part 2 of the series) and
the parser (part 3) are generated from [`pl0.pg`](pl0.pg), together with AST
types and quasi-quoters that the upcoming passes are written against.

**Status: parts 1–3** — the series' "validator" milestone: `pl0 file.pl0`
parses baseline Wirth PL/0 and prints the AST, with positioned diagnostics
for lexical and syntax errors. The original is single-pass (its parser emits
C as it goes); this implementation is deliberately multi-pass (parse → AST →
check → emit), which is what the quasi-quoters are for.

## Layout

| File | Role |
|------|------|
| `pl0.pg` | The PL/0 grammar. Everything under `gen/` is generated from it. |
| `Main.hs` | Validator driver: `pl0 [--lex-only] file.pl0` parses and pretty-prints the AST. |
| `TestQQ.hs` | Tests of the full QQ feature set, including a miniature PL/0 → C code generator and SYB rewrite rules over the generated AST. |
| `run_tests.sh` | Checks every program under `tests/valid` parses and every program under `tests/invalid` is rejected with a positioned error. |

## Building and testing

The tutorial borrows all Haskell tooling from the RTK checkout two levels up
(`cabal exec rtk/alex/happy/ghc`), so build RTK first:

```bash
cd ../..
cabal build          # plus the toolchain setup described in /CLAUDE.md
cd tutorials/pl0-compiler

make build           # pl0.pg -> gen/{Pl0Lexer,Pl0Parser,Pl0QQ} -> pl0, test-qq
make test            # QQ feature tests + parser tests
```

## The grammar, in RTK idioms

- The expression hierarchy uses `,`-lifted pass-throughs
  (`Expression: Term = ... | ,Factor ;`), so `Expression`, `Term` and
  `Factor` share one AST type with no wrapper constructors, parentheses are
  transparent, and one QQ pattern matches any expression node.
- Wirth's grammar makes every statement position optional
  (`statement = [ ... ]`); `StatementOpt = Statement? ;` is that bracketed
  statement as a named rule, so `begin end.`, a trailing `;` before `end`,
  and the minimal program `.` all parse, and `$so1` metavariables bind
  either an empty or a present statement.
- `Integer: number = [0-9]+ ;` gives the number token a real `Integer`
  payload; `@shortcuts` declarations (`e`, `s`, `so`, `c`, `id`, `n`) name
  the metavariables.
- The parser is conflict-free (no shift/reduce, no reduce/reduce), and the
  grammar generates byte-identically under both of RTK's front ends.

A faithfulness detail worth knowing: Wirth allows a sign only at the *start*
of an expression (`expression = [+|-] term {(+|-) term}`), so `-a + +b` is
invalid PL/0 — `tests/invalid/sign-in-continuation.pl0` pins exactly that.

## What "full RTK power" looks like here

`TestQQ.hs` contains a miniature of the upcoming C code generator, written
entirely as QQ patterns — no generated constructor names in sight:

```haskell
cgStmt [statement| $id1 := $e1 |]       = cgIdent id1 ++ " = " ++ cgExpr e1 ++ ";"
cgStmt [statement| while $c1 do $so1 |] = "while " ++ cgCond c1 ++ " " ++ cgStmtOpt so1

cgExpr [expression| $e1 + $e2 |]        = "(" ++ cgExpr e1 ++ " + " ++ cgExpr e2 ++ ")"
```

construction and splicing:

```haskell
let eLhs = [expression| x + 1 |]
    eRhs = [expression| y - 1 |]
in  [expression| $eLhs * $eRhs |]       -- == [expression| (x + 1) * (y - 1) |]
```

and AST rewrite rules, with QQ patterns on the left and QQ values on the
right, lifted over whole trees by SYB:

```haskell
simplify [expression| $e1 + 0 |] = e1
simplify [expression| $e1 * 1 |] = e1
simplify x = x

optimize = everywhere (mkT simplify)    -- (x + 0) * 1 + y * 1  ~>  x + y
```

The grammar-convention and limitation notes in
[`../c-compiler/README.md`](../c-compiler/README.md) apply here unchanged;
`pl0.pg` keeps the *scalar* antiquote shape for `Statement` (no named list
rule), so `$s1` binds one statement and `begin` blocks are matched
positionally (`begin $s1 ; $s2 end`) or through the list constructor.

## Roadmap

[`PLAN.md`](PLAN.md) breaks the remaining work into self-contained task
blobs, each written to be pasted into a fresh session as the task
description. In dependency order:

1. semantic analysis (`Check.hs`): pl0c's check set with line-numbered
   diagnostics via the AST's `RtkPos` positions
2. the C code generator (series part 5): `pl0 file.pl0` → C → `cc` →
   runnable binary, with `run_tests.sh` executing every valid program
3. I/O statements (part 6): `writeInt`, `readInt into`, ... — sample
   programs gain `.expected` outputs
4. arrays (part 7): `size` declarations, indexing, array/scalar checks
5. strings, `forward`, `exit`, `and`/`or`/`not`, `mod` (part 8) —
   pl0c feature parity
6. an optimizer pass (`-O`): QQ rewrite rules + SYB, the pass the
   single-pass C original structurally cannot have
7. (core RTK) scalar and list antiquotes for one type — lifts the
   `$stmts` whole-list-binder limitation both tutorials design around
8. `TUTORIAL.md`: the original series retold with RTK, part by part —
   original prose, sections linking each source post, every snippet
   copied from the working files (startable now for parts 1-3)

This directory deliberately depends on the parent checkout only through
`cabal exec`; giving it its own `.cabal` file is all it takes to move it to
its own repository.
