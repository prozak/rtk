# Writing a C Compiler — with RTK

An implementation of Nora Sandler's
["Writing a C Compiler"](https://norasandler.com/2017/11/29/Write-a-Compiler.html)
tutorial in which **both languages the compiler touches are RTK grammars**:
the C front end (lexer, parser, AST types, quasi-quoters) is generated from
[`c.pg`](c.pg), and the x86-64 assembly it emits is itself an RTK grammar
([`asm.pg`](asm.pg)) — code generation builds an assembly AST with
quasi-quotation splices instead of concatenating strings, and a generated
assembly *parser* (a by-product) round-trip-tests the emitter.

**Status: stage 1** (proof of concept): `int main() { return <int>; }` →
assembly AST → AT&T text → executable via gcc. Verified against the official
[stage-1 test suite](https://github.com/nlsandler/write_a_c_compiler)
(12/12) in addition to the local tests under [`tests/`](tests/).

## Layout

| File | Role |
|------|------|
| `c.pg` | The C grammar (input language). |
| `asm.pg` | The assembly grammar (output language). Everything under `gen/` is generated from these two. |
| `Main.hs` | Compiler driver: `ncc file.c` produces an executable next to the source (the tutorial's test-suite contract), assembling/linking through gcc. |
| `Codegen.hs` | C AST → assembly AST; QQ patterns on the C side, QQ construction + splices on the assembly side. |
| `Emit.hs` | Assembly AST → AT&T text (RTK generates parsers, not pretty-printers; this is the hand-written half, kept honest by the round-trip test). |
| `TestQQ.hs` | End-to-end tests of the full QQ feature set for both grammars, plus the emit/parse round trip. |
| `run_tests.sh` | Compiles `tests/valid`/`tests/invalid` and checks exit codes against a gcc-built reference. |

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

## Known RTK limitations to design around

- **One antiquote shape per AST type** (scalar or list, whichever is
  normalized first — see `_antiRuleCache` in `Normalize.hs`). `c.pg` orders
  `StatementList = Statement*` before `Statement`, so type `Statement` gets
  the *list* shape: `$stmts` binds/splices a whole `[Statement]`, while a
  scalar `$statement1` antiquote would misbehave silently. Scalar antiquotes
  are fine for types never used in a list rule (`$e`, `$name`, `$src`,
  `$sym`).
- **List antiquotes in patterns bind the whole list only** (`{ $stmts }`);
  mixed list patterns (`{ $stmts return 0 ; }`) only work in construction.
- **Token payloads cannot be antiquoted.** `$x` splices/matches whole syntax
  sorts; the `Int` of an `intLit` or the `String` of an `id` is reached
  through the generated constructors (`Ctr__Exp__0 _ n`, `Ctr__Ident__0 _ s`;
  the first field is the node's source position) — see `expValue`/`identName`
  in `Codegen.hs`. Nodes built in code take `rtkNoPos` there; AST equality
  ignores positions by design, so built and parsed trees still compare equal.
- **Quoter names are lowercased rule names** and can collide with Prelude
  names: the `Exp` quoter is `exp`, hence `import Prelude hiding (exp)`.
  Avoid rule names that lowercase to Haskell keywords (`If`, `Do`, `Type`...).

## Roadmap

Following the blog series, one stage at a time:

2. unary operators (`-` `~` `!`)
3. binary operators `+ - * /` and the precedence cascade
4. relational/logical operators, short-circuit evaluation
5. local variables (first semantic pass: variable resolution)
6. `if`/`else` and the conditional expression
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
