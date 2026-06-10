# Tutorials

Self-contained example projects built **with** RTK (as opposed to the grammars
under `test-grammars/`, which test RTK itself). Each tutorial lives in its own
directory and only reaches into the parent checkout for the RTK toolchain
(`cabal exec rtk/alex/happy/ghc`), so a tutorial can later be extracted into a
separate repository with minimal surgery.

## Contents

- [`c-compiler/`](c-compiler/) — an implementation of Nora Sandler's
  ["Writing a C Compiler"](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
  tutorial where both languages are RTK grammars: the C front end (lexer,
  parser, AST, quasi-quoters) is generated from `c.pg`, and code generation
  builds an AST of the `asm.pg` assembly grammar via quasi-quotation splices
  instead of emitting strings. Currently at stage 1 (proof of concept).
- [`write-you-a-haskell/`](write-you-a-haskell/) — the languages of Stephen
  Diehl's ["Write You a Haskell"](https://github.com/sdiehl/write-you-a-haskell)
  tutorial as RTK grammars: four interpreters (untyped lambda calculus,
  simply typed LC, Poly with Hindley-Milner inference, and ProtoHaskell-lite
  with algebraic data types and case), where every semantic pass - desugaring,
  type checking, algorithm W, evaluation - pattern-matches concrete syntax
  via the generated quasi-quoters. Each language ships a test suite and a
  REPL.
- [`lets-build-a-compiler/`](lets-build-a-compiler/) — an implementation of
  Jack Crenshaw's ["Let's Build a Compiler"](https://compilers.iecc.com/crenshaw/)
  tutorial: the hand-rolled recursive descent parser the tutorial spends most
  of its chapters on is replaced by an RTK grammar, and the compiler passes
  work on the generated AST through quasi-quotation patterns and splices.
  Currently at milestone 0 (the parts 2-4 expression language plus a
  QQ-pattern interpreter).
- [`pl0-compiler/`](pl0-compiler/) — an implementation of Brian Callahan's
  ["Let's write a compiler"](https://briancallahan.net/blog/20210814.html)
  tutorial series (PL/0): the lexer and parser of the original — most of its
  hand-written C — are generated from `pl0.pg`, and the quasi-quotation test
  suite doubles as a miniature of the upcoming PL/0 → C code generator (QQ
  pattern matching, splices, and SYB rewrite rules over the generated AST).
  Currently at parts 1-3 (the series' parser/"validator" milestone).
