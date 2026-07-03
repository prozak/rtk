# Writing a C Compiler, with RTK — the companion tutorial

This is a page-by-page retelling of Nora Sandler's
[**Writing a C Compiler**](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
series, building the same compiler — but with [RTK](../../../README.md)
generating the front end and the assembly emitter from grammar files instead
of hand-writing them.

It is a *companion*, not a replacement. Each page assumes you have the
corresponding blog post (or the [No Starch book](https://nostarch.com/writing-c-compiler))
open: Sandler teaches how a compiler works; these pages teach the **RTK
delta** — what you no longer write by hand (the lexer, the parser, the AST
types, the boilerplate that walks it) and what you write instead (grammar
rules, quasi-quotation patterns, antiquote splices). At each point where the
blog forks from this implementation, the page puts the two side by side.

The working code under [`tutorials/c-compiler/`](../) is the source of truth;
every snippet and every block of output on these pages is taken from it and
verified against the build.

## Pages

| Page | Blog part | Covers |
|------|-----------|--------|
| [00 — Setup](00-setup.md) | (preamble) | The toolchain, the pipeline, the driver contract, and how to build and test. |
| [01 — Integers](01-integers.md) | [Part 1](https://norasandler.com/2017/11/29/Write-a-Compiler.html) | `int main() { return 2; }` → an executable. The whole pipeline, end to end. |
| [02 — Unary operators](02-unary.md) | [Part 2](https://norasandler.com/2017/12/05/Write-a-Compiler-2.html) | `-`, `~`, `!` and nesting; codegen becomes recursive. |
| [03 — Binary operators](03-binary.md) | [Part 3](https://norasandler.com/2017/12/15/Write-a-Compiler-3.html) | `+ - * /`, the precedence cascade, and a stack-machine codegen. |
| [04 — Relational and logical](04-relational.md) | [Part 4](https://norasandler.com/2018/01/08/Write-a-Compiler-4.html) | `== != < <= > >= && ||`, short-circuiting, and the first stateful codegen. |
| [05 — Local variables](05-variables.md) | [Part 5](https://norasandler.com/2018/01/25/Write-a-Compiler-5.html) | Declarations, assignment, a stack frame, and the first semantic pass (SYB). |
| [06 — if/else and ?:](06-conditionals.md) | [Part 6](https://norasandler.com/2018/02/25/Write-a-Compiler-6.html) | The statement/declaration split, the dangling else (a pinned, intentional conflict), and jump diamonds. |
| [07 — Blocks and scoping](07-blocks.md) | [Part 7](https://norasandler.com/2018/03/14/Write-a-Compiler-7.html) | Compound statements, the scope stack, and alpha-renaming: the first pass that rewrites the tree (monadic SYB). |

Stages 8–10 (loops through file-scope variables) are implemented one at a
time; each adds a page here. Until then they live as task descriptions in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).

## Conventions on these pages

- **Blog ⇄ RTK** call-outs mark each fork between Sandler's approach and this
  one.
- Shell blocks show real commands; output blocks are copied from an actual
  run.
- Where RTK costs something — a limitation or an awkwardness — the page says
  so at the point you would hit it, not in an appendix. The
  [project README](../README.md) collects these as a reference.
