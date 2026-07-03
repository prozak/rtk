# Writing a C Compiler — with RTK

An implementation of Nora Sandler's
["Writing a C Compiler"](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
tutorial in which **both languages the compiler touches are RTK grammars**:
the C front end (lexer, parser, AST types, quasi-quoters) is generated from
[`c.pg`](c.pg), and the x86-64 assembly it emits is itself an RTK grammar
([`asm.pg`](asm.pg)) — code generation builds an assembly AST with
quasi-quotation splices instead of concatenating strings, and a generated
assembly *parser* (a by-product) round-trip-tests the emitter.

**Status: stage 6**: integer `return`, the unary `-` `~` `!`, binary `+ - * /`
(precedence cascade, parentheses), relational/logical `== != < <= > >= && ||`
with short-circuiting, local variables (declarations, assignment, references)
with a stack frame and a name-resolution semantic pass, and control flow:
`if`/`else` (with the dangling-else conflict resolved and pinned) and the
ternary `?:`, over a statement/declaration split that makes
`if (5) int i = 0;` a syntax error. C source → resolve → assembly AST → AT&T
text → executable via gcc. Verified against the official
[test suite](https://github.com/nlsandler/write_a_c_compiler) (stage 1: 12/12,
2: 11/11, 3: 16/16, 4: 27/27, 5: 17/17, 6: 24/24 — 107/107) in addition to the
local tests under [`tests/`](tests/).

## Companion tutorial

[`tutorial/`](tutorial/) retells Nora Sandler's series page by page with RTK —
what the generator replaces (lexer, parser, AST, the boilerplate that walks
it) and what you write instead (grammar rules, quasi-quotation patterns,
splices). Start at the [index](tutorial/README.md): stages 1–6 are covered by
[00 — Setup](tutorial/00-setup.md), [01 — Integers](tutorial/01-integers.md),
[02 — Unary operators](tutorial/02-unary.md),
[03 — Binary operators](tutorial/03-binary.md),
[04 — Relational and logical](tutorial/04-relational.md),
[05 — Local variables](tutorial/05-variables.md), and
[06 — if/else and ?:](tutorial/06-conditionals.md).
This README is the reference companion to those pages: it catalogues the
conventions and limitations below, which the pages link to as you hit them.

## Layout

| File | Role |
|------|------|
| `c.pg` | The C grammar (input language). |
| `asm.pg` | The assembly grammar (output language). Everything under `gen/` is generated from these two. |
| `Main.hs` | Compiler driver: `ncc file.c` produces an executable next to the source (the tutorial's test-suite contract), assembling/linking through gcc. |
| `Resolve.hs` | Semantic pass: resolves variable names to stack slots and rejects undeclared/redeclared variables. QQ for matching, SYB (`listify`) for the whole-tree query. |
| `Codegen.hs` | C AST → assembly AST; QQ patterns on the C side, QQ construction + splices on the assembly side. |
| `Emit.hs` | Assembly AST → AT&T text (RTK generates parsers, not pretty-printers; this is the hand-written half, kept honest by the round-trip test). |
| `TestQQ.hs` | End-to-end tests of the full QQ feature set for both grammars, plus the emit/parse round trip. |
| `run_tests.sh` | Compiles `tests/valid`/`tests/invalid` and checks exit codes against a gcc-built reference. |

`make test` also runs `conflict-check`, which pins the parsers' LALR conflict
inventory: exactly one shift/reduce in `CParser` (the dangling else — shift
binds the `else` to the nearest `if`, the C rule) and one in `AsmParser` (the
quasi-quoter bootstrap dummy-bracket conflict every generated grammar's
whole-file entry carries), zero reduce/reduce anywhere. A grammar change that
adds a conflict cannot hide behind the expected ones.

## Building and testing

The tutorial borrows all Haskell tooling from the RTK checkout two levels up
(`cabal exec rtk/alex/happy/ghc`), so build RTK first:

```bash
cd ../..
cabal build          # plus the toolchain setup described in /CLAUDE.md
cd tutorials/c-compiler

make build           # c.pg -> gen/{CLexer,CParser,CQQ} -> ncc, test-qq
make test            # QQ feature tests + compiler tests
```

To run the official tutorial test suite:

```bash
git clone https://github.com/nlsandler/write_a_c_compiler /tmp/write_a_c_compiler
cd /tmp/write_a_c_compiler && ./test_compiler.sh /path/to/this/dir/ncc 1
```

## What "full RTK power" looks like here

Pattern matching with antiquote binders, in `Codegen.hs`:

```haskell
codegen [program| int $name ( ) { $stmts } |] = ...   -- $stmts :: [Statement]

genStatement [statement| return $e ; |] = ...         -- $e :: Exp
```

Construction with splices (`TestQQ.hs` verifies all of these):

```haskell
let e = [exp| 42 |]
in [statement| return $e ; |]                         -- scalar splice

let stmts0 = [[statement| return 1 ; |]]
in [program| int main ( ) { $stmts0 return 9 ; } |]   -- list splice, mixed
```

And the same machinery on the *output* language — codegen assembles its
result instead of printing strings:

```haskell
genStatement [statement| return $e ; |] =
  let src = mkImm (expValue e)
  in [asmItems|
       movl $src, %eax
       ret
     |]
```

Real `$2` immediates and `$src` antiquotes coexist in the assembly quotes:
immediates are always numeric, antiquote names always start with a letter.

## Grammar conventions that make QQ work (learned the hard way)

The Java grammar in `test-grammars/java.pg` had construction-only QQ for a
long time; the causes are avoidable, and `c.pg` is written to avoid them:

1. **Use `,`-lifted pass-throughs in precedence cascades** (as in
   `test-grammars/grammar.pg`) so every level shares one `Exp` type without
   per-level wrapper constructors — that is what lets one QQ pattern match
   any expression node directly. (Where the `$`-splice token attaches is
   handled by RTK's unit-production cover analysis regardless of style; see
   `Normalize.hs`.) Relevant from stage 3 on, when `Exp` becomes a cascade.
2. **Declare `@shortcuts` for the names you want in antiquotes.** `$e1`
   resolves by longest prefix against rule shortcuts; without `@shortcuts(e)`
   on `Exp` it fails at compile time with "Unknown shortcut".
3. **Keep `$` out of identifier tokens.** Java identifiers may contain `$`,
   which collides with the `$Type:name` antiquote tokens. C identifiers
   don't — nothing to do, but worth knowing.

4. **Two generated parsers can live in one program, with care.** Sort names
   must be globally distinct (`asm.pg` uses `AsmItem`/`AsmId`/... because
   `CParser` already owns `Program`/`Ident`/...). The two QQ modules export
   identically-named internals (`replaceAllPatterns`, `qqShortcuts`, ...) and
   the parsers share `parseError`/`showRtkToken`; that only clashes at use
   sites, so importing both unqualified works as long as you only touch the
   quoters, types, and parse functions.
5. **Antiquote names need a following non-name, non-colon character.**
   `$sym :` parses as a label with an antiquoted symbol, but `$sym:` is read
   as the explicit `$Rule:name` antiquote form and won't scan.
6. **Name the alternatives' constructors.** A leading label
   (`Exp = IntLit: intLit ;`) names the generated AST constructor; without it
   the name is positional (`Ctr__Exp__0`), encoding the alternative's index,
   so reordering the grammar silently renames it. `c.pg`/`asm.pg` name every
   alternative, which is what lets the passes read `IntLit _ n` instead of
   `Ctr__Exp__0 _ n`. Names must be unique across the grammar — and, by the
   same unqualified-import reasoning as (4), across both grammars: the C side
   uses `IntLit`/`Return`/`Name`/..., the asm side `Imm`/`Movl`/`Sym`/....
   (The start sort keeps one auto-generated `Ctr__*` wrapper for the
   quasi-quoter scaffolding; no pass references it.)

## Known RTK limitations to design around

- **One antiquote shape per AST type** (scalar or list, whichever is
  normalized first — see `_antiRuleCache` in `Normalize.hs`, and #162 for the
  planned fix). `c.pg` orders `BlockItemList = BlockItem*` before `BlockItem`,
  so type `BlockItem` gets the *list* shape: `$stmts` binds/splices a whole
  `[BlockItem]`, while a scalar `$blockItem1` antiquote would misbehave
  silently. Scalar antiquotes are fine for types never used in a list rule
  (`$e`, `$s`, `$name`, `$src`, `$sym`) — and shapes can *move*: through
  stage 5 `Statement` was the list type, and the stage-6 statement/declaration
  split handed the list position to `BlockItem`, flipping `Statement` to
  scalar (which is what the if/else codegen patterns want).
- **List antiquotes in patterns bind the whole list only** (`{ $stmts }`);
  mixed list patterns (`{ $stmts return 0 ; }`) only work in construction.
- **Token payloads cannot be antiquoted.** `$x` splices/matches whole syntax
  sorts; the `Int` of an `intLit` or the `String` of an `id` is reached
  through the alternative's named constructor (`IntLit _ n`, `Name _ s`; the
  first field is the node's source position) — see the `IntLit` match in
  `genExp` and `identName` in `Codegen.hs`. Nodes built in code take `rtkNoPos`
  there; AST equality ignores positions by design, so built and parsed trees
  still compare equal.
- **Quoter names are lowercased rule names** and can collide with Prelude
  names: the `Exp` quoter is `exp`, hence `import Prelude hiding (exp)`.
  Avoid rule names that lowercase to Haskell keywords (`If`, `Do`, `Type`...).

## Roadmap

Following the blog series, one stage at a time. Stages 1–6 (integers, unary
operators, binary operators with the precedence cascade, relational/logical
operators with short-circuiting, local variables with a name-resolution
semantic pass, and if/else with the conditional expression) are done; up next:

7. compound statements and scoping
8. loops, `break`/`continue`
9. function calls (System V calling convention)
10. global variables

Each stage — plus the RTK-side follow-ups this PoC surfaced (both antiquote
shapes per type, mixed list patterns, vendoring the official suite, named
constructors, a companion tutorial retelling the blog series with RTK,
extraction to a separate repository) — is written up as a self-contained
task blob in
[`docs/c-compiler-tutorial-plan.md`](../../docs/c-compiler-tutorial-plan.md);
pick the next one from there.

This directory deliberately depends on the parent checkout only through
`cabal exec`; giving it its own `.cabal` file (depending on alex/happy/ghc
plus `syb` for the generated QQ modules) is all it takes to move it to its
own repository.
