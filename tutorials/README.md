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
