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
