# Plan: "Writing a C Compiler" tutorial — stages 2–10 and RTK follow-ups

Status: PLANNED (follow-up to the stage-1 PoC + asm.pg conversion, PR #93).

The tutorial under `tutorials/c-compiler/` implements stage 1 of Nora
Sandler's blog series with both languages as RTK grammars: `c.pg` (input)
and `asm.pg` (output), QQ patterns destructuring the C AST, QQ construction
with `$`-splices building the assembly AST, a hand-written `Emit.hs` kept
honest by a parse-back round trip, and two test layers (`TestQQ.hs` for the
QQ feature matrix, `run_tests.sh` against gcc as the reference; the official
suite's stage 1 passes 12/12). This file breaks the remaining work into task
blobs in dependency order: C2–C10 follow the blog parts one stage per task
and are sequential; the R tasks are independent improvements that can be
interleaved. Each blob is written to be pasteable into a fresh session as
the task description.

Blog: https://norasandler.com/2017/11/29/Write-a-Compiler.html (parts 1–10).
Official tests: https://github.com/nlsandler/write_a_c_compiler
(`./test_compiler.sh /path/to/ncc N` runs stage N; the compiler contract is
`ncc file.c` producing an executable next to the source, nothing on
rejection).

Invariants every task must preserve:

- `make -C tutorials/c-compiler test` green (QQ matrix + compiler tests),
  and the official suite green for every stage implemented so far.
- The tutorial reaches into the parent checkout only via `cabal exec`
  (it must stay extractable, see task R5).
- The root battery stays green; RTK core changes only when a task
  explicitly says so, and then with `make accept-golden` + reviewed diff.
- The grammar conventions documented in `tutorials/c-compiler/README.md`
  (distinct sort names across c.pg/asm.pg, `@shortcuts` for antiquote
  names, list rule normalized before its element rule, no Prelude/keyword
  quoter-name collisions, `$sym :` spacing).
- Per stage: extend the grammar(s) → regenerate → extend `TestQQ.hs` for
  every new sort/quoter you rely on → extend the passes → local tests →
  official suite → extend the companion tutorial page for the stage (task
  R6, once it exists). One commit per stage.

---

## Task C2 — Unary operators (blog part 2)

### TL;DR

Compile `return -2;`, `return ~7;`, `return !5;`: nested unary expressions,
first recursive codegen.

### Changes

- `c.pg`: `UnaryOp = '-' | '~' | '!' ;` and `Exp = UnaryOp Exp | intLit ;`.
  Factoring the operator into its own sort keeps one constructor per shape,
  so codegen can bind it generically: `[exp| $unaryOp1 $e1 |]`.
- `asm.pg`: instructions `neg`, `notl`, `cmpl`, `sete`; the byte register
  `%al` joins `Reg` (logical not is `cmpl $0,%eax; movl $0,%eax; sete %al`).
- `Codegen.hs`: `genExp :: Exp -> [AsmItem]` becomes recursive (value in
  `%eax`); dispatch on the `UnaryOp` sort.
- Tests: local valid (nested ops, e.g. `!!5`, `-~2`) and invalid
  (`return -;`); TestQQ gains unary construction/pattern cases; official
  stage 2 (`./test_compiler.sh ncc 2` — keep 1 green too).

### Gotchas

- `intLit` carries no sign: `-2` lexes as `-` then `2` — that is the
  tutorial's intent (constant folding is out of scope).
- `'!'` is the ignore marker in .pg syntax rules, but a quoted `'!'`
  literal is just a token (grammar.pg itself does this).

## Task C3 — Binary operators and the precedence cascade (blog part 3)

### TL;DR

`+ - * /` with correct precedence and parentheses. This is where the
`Exp` precedence cascade lands; everything later (parts 4–6) extends it.

### Changes

- `c.pg`: the lifted cascade with shared type and op sorts:

      @shortcuts(e)
      Exp = ,AddExp ;
      Exp: AddExp = AddExp AddOp MulExp | ,MulExp ;
      Exp: MulExp = MulExp MulOp UnaryExp | ,UnaryExp ;
      Exp: UnaryExp = UnaryOp UnaryExp | ,Factor ;
      Exp: Factor = intLit | '(' ,Exp ')' ;
      AddOp = '+' | '-' ;   MulOp = '*' | '/' ;

  `,`-lifting keeps every level in the single `Exp` type with no wrapper
  constructors, so `[exp| $e1 $addOp1 $e2 |]` and `[exp| $e1 $mulOp1 $e2 |]`
  match one constructor per precedence level. Splice-token placement is
  RTK's unit-production cover analysis; `--analyze-conflicts` should stay
  quiet.
- `asm.pg`: `addl`, `subl`, `imull`, `cdq`, `idivl`, `push`/`pop` with the
  64-bit names (`%rax`, `%rcx`) — push/pop only exist at 64-bit width, so
  `Reg` now spans both widths (the emitter just prints the name).
- `Codegen.hs`: blog-style stack machine — evaluate left, `push %rax`,
  evaluate right, `pop %rcx`, apply. Mind AT&T operand order for `subl`
  and the `cdq`/`idivl` pair for division.
- Tests: precedence/associativity cases checked against gcc exit codes
  (`2+3*4`, `(2+3)*4`, `10-3-4`, division truncation); official stage 3.

### Gotchas

- `-` is both `UnaryOp` and `AddOp` — same token in two sorts is fine
  (sorts are syntax rules, the token is shared).
- After this stage the README's cascade advice is load-bearing; if QQ
  patterns misbehave, inspect the generated `CParser.y` attach points
  before suspecting the passes.

## Task C4 — Relational and logical operators, short-circuit jumps (blog part 4)

### TL;DR

`&& || == != < <= > >=` with C's short-circuit semantics: codegen learns
conditional jumps and needs a unique-label supply (it becomes stateful).

### Changes

- `c.pg`: cascade grows on top of AddExp: `LOrExp` → `LAndExp` → `EqExp`
  (`EqOp = '==' | '!='`) → `RelExp` (`RelOp = '<' | '>' | '<=' | '>='`).
  `&&`/`||` get their own constructors (they are control flow, not
  operators over values).
- `asm.pg`: labels as operandless items already exist (`AsmId ':'`); add
  `cmpl`, `jmp`, `je`/`jne`, and the `setcc` family (`sete setne setl setle
  setg setge`).
- `Codegen.hs`: thread a label counter (State monad or explicit supply);
  generated names like `_clause3`/`_end3` are leaves built with `mkSym`
  (token payloads cannot be antiquoted — already documented). Comparison
  codegen: `cmpl`, zero `%eax`, `setcc %al`.
- Tests: short-circuit behavior is observable through exit codes
  (`1 || (1/0)` exits 1 without crashing); official stage 4.

### Gotchas

- The State refactor ripples through every gen function's signature — do
  it first, mechanically, before adding the new operators.
- Token overlap (`<` vs `<=`, `=` vs `==`, `&&` vs future `&`): alex
  maximal munch handles it; keep an eye on `make test-lex`-style spot
  checks in TestQQ instead of trusting it blindly.

## Task C5 — Local variables: the first semantic pass (blog part 5)

### TL;DR

`int a = 2; a = a + 3; return a;` — declarations, assignment, variable
references, stack slots, and the compiler's first validation pass (with a
new "reject semantically invalid programs" driver path).

### Changes

- `c.pg`: `Statement = 'return' Exp ';' | Exp ';' | Declaration ;`,
  `Declaration = 'int' Ident ('=' Exp)? ';'`, `Factor` gains `Ident`, and
  assignment enters the cascade top: `Exp = Ident '=' Exp | ,LOrExp ;`
  (the blog's grammar; the id-lhs form keeps LALR happy since `=` and `==`
  are distinct tokens).
- New pass module (e.g. `Resolve.hs`): walks the AST with QQ patterns,
  building the var→stack-offset map; rejects duplicate declarations and
  undeclared uses with `Either String`. `Main.hs` runs it between parse and
  codegen — same contract as parse errors: non-zero exit, no artifacts.
- `asm.pg`: memory operands `-4(%rbp)` (negative offset, paren form) and
  `%rbp`/`%rsp`; function prologue/epilogue in codegen (every `ret` goes
  through the epilogue).
- Tests: locals/assignment/missing-return (`int main(){int a=1;}` exits 0 —
  the existing fall-off-the-end default already covers it); invalid:
  redeclaration, undeclared variable, assignment to non-lvalue; official
  stage 5.

### Gotchas

- This is the stage that shows whether QQ patterns scale to a real pass;
  if generic traversal gets noisy, `Data.Generics` (`everywhereM`) over the
  derived `Data` instances is the idiomatic escape hatch — use it
  deliberately and document the mix.
- Offsets are `Int` leaves: extend `mkImm`-style helpers for memory
  operands rather than scattering `Ctr__Operand__N` literals.

## Task C6 — if/else and the conditional expression (blog part 6)

### TL;DR

`if`/`else` statements and the ternary `?:` — the first intentional LALR
conflict (dangling else) and right-associative grammar.

### Changes

- `c.pg`: `Statement = ... | 'if' '(' Exp ')' Statement ('else' Statement)? ;`
  and the ternary level: `Exp: CondExp = LOrExp '?' Exp ':' CondExp | ,LOrExp ;`
  (right-recursion = right-associativity).
- Codegen: plain jump patterns reusing the C4 label supply.
- Tests: nested if/else binding (`if (a) if (b) ... else ...` — else binds
  to the nearest if), ternary nesting; official stage 6.

### Gotchas

- Dangling else is a genuine shift/reduce conflict resolved correctly by
  happy's shift preference. Record the expected conflict count in the
  tutorial README (and assert on the happy `-i` info file if cheap) so a
  later grammar change that adds conflicts doesn't hide behind this one.

## Task C7 — Compound statements and scoping (blog part 7)

### TL;DR

Blocks as statements, declarations scoped to their block, shadowing.

### Changes

- `c.pg`: `Block = '{' BlockItemList '}' ;`, `BlockItemList = BlockItem* ;`,
  `BlockItem = Statement | Declaration ;`; `Function` body becomes `Block`;
  `Statement` gains `Block`. Keep the list rule before `BlockItem` (list
  antiquote shape — same reasoning as `StatementList`, which this replaces).
- `Resolve.hs`: scope stack (list of maps); duplicate-declaration is an
  error only within the same scope; offsets keep growing monotonically
  (the blog's scheme).
- Tests: shadowing returns the inner value, use-after-scope-exit rejected;
  official stage 7.

### Gotchas

- This restructures stage-1-era grammar (StatementList → BlockItemList);
  TestQQ's `$stmts` cases move to the new sort — expect to touch most QQ
  tests once, mechanically.

## Task C8 — Loops and break/continue (blog part 8)

### TL;DR

`for`, `while`, `do`-`while`, `break`, `continue` — plus a second
annotation pass associating `break`/`continue` with their enclosing loop.

### Changes

- `c.pg`: the three loop forms (`for` with `ForInit = Declaration | OptExp ';'`
  and optional condition/post expressions), `'break' ';'`, `'continue' ';'`.
  Optional-expression slots are `(Exp)?` — RTK turns those into
  empty/non-empty constructors, fine for patterns.
- Loop-labeling pass (can live in `Resolve.hs`): each loop gets a label pair
  from the supply; `break`/`continue` outside any loop is a semantic error.
- Codegen: standard loop skeletons; `for` introduces its own scope for the
  init declaration (C99 — the official suite's `for_nested_scope` case had
  UB fixed upstream, compare against a recent clone).
- Tests: all three loop forms, nested loops with break/continue; invalid:
  `break;` at top level; official stage 8.

## Task C9 — Function calls and the System V ABI (blog part 9)

### TL;DR

Multiple functions, declarations vs definitions, calls with arguments —
the ABI stage (argument registers, stack alignment, calling libc).

### Changes

- `c.pg`: `Program = FunctionList ;` with `FunctionList = Function* ;`
  (list before element — `Function`'s antiquote shape flips to list;
  scalar `$function1` splices stop being available, note it in the README),
  `Function = 'int' Ident '(' ParamList? ')' ( Block | ';' ) ;`,
  `Factor` gains calls: `Ident '(' ArgList ')'` (LALR disambiguates
  variable vs call on the `(` lookahead).
- Semantic checks: call arity matches declaration, no redefinition,
  declarations consistent.
- `asm.pg`: `call`, argument registers (`%edi %esi %edx %ecx %r8d %r9d`),
  stack alignment. Keep `main`'s 16-byte alignment before `call` — the
  blog's scheme (pad to multiple of 16 counting pushed args) is enough.
- Tests: multi-function programs, recursion (fibonacci), `putchar` —
  stage 9's `hello_world.c` calls libc via gcc's default linking; official
  stage 9.

### Gotchas

- This stage stresses the two-parser name discipline: nothing new should
  clash, but new asm sorts (arg registers) belong in `Reg`, not new sorts.

## Task C10 — File-scope variables (blog part 10)

### TL;DR

Global variables: `.data`/`.bss` sections, symbol-relative addressing,
initializer rules.

### Changes

- `c.pg`: `Program` items become function-or-declaration; global
  `Declaration` initializers must be integer constants (semantic check).
- `asm.pg`: directives `.data`, `.bss`, `.align`, `.zero`/`.long` (or
  `.comm`), and symbol operands (`x(%rip)` form on x86-64).
- Codegen/Resolve: identifier resolution distinguishes stack slots from
  symbols; uninitialized globals go to `.bss`, initialized to `.data`.
- Tests: globals read/written across functions, initialized and not;
  invalid: non-constant initializer, conflicting definitions; official
  stage 10 — at which point the whole blog series is done and the README's
  status section flips to "complete".

---

## Task R1 — Vendor the official test suite and gate stages in CI

The local `tests/` are a fast smoke layer; the real acceptance is
nlsandler's suite, currently cloned ad hoc. Add it the way commons-lang is
vendored (git submodule under `test-suites/`), a
`make -C tutorials/c-compiler test-official STAGES="1 2 ..."` target that
runs `test_compiler.sh` for every implemented stage, and a CI entry. Keep
the stage list in one place (a variable in the tutorial Makefile that each
C-task bumps). Mind CI checkout already uses `submodules: recursive`.

## Task R2 — Support both antiquote shapes per type (RTK core)

`Normalize._antiRuleCache` keys by type name only, so a type gets either
the scalar or the list antiquote functions — whichever rule normalized
first (the tutorial orders list rules first and forgoes scalar `$stmt1`
antiquotes as a workaround, documented in its README). The lexer tokens
are already distinct (`qq_Statement` vs `qq_StatementList`); what's missing
is per-shape anti constructors (e.g. `Anti_X` and `AntiList_X`) and GenQ
emitting both function families. Acceptance: a grammar with `X` used both
standalone and in `X*` supports `$x1` and `$xs` simultaneously; the
tutorial drops its ordering workaround and the README paragraph; goldens
regenerated; the i14/p QQ runtime tests extended.

## Task R3 — List antiquotes in pattern mode beyond whole-list binding (RTK core)

`listPatGen` only matches `[Anti_X v]` — a list pattern is either fully
literal or one variable. The construction side already supports mixed
splices (`{ $stmts0 return 9 ; }`). A useful, cheap pattern-side extension:
anti at the *end* binds the tail (`{ return 0 ; $rest }` becomes literal
cons cells with `varP rest` as the tail); anti at the head would need
suffix matching and is likely not worth it. Decide the scope in-session;
update the tutorial README's limitation bullet and TestQQ either way.

## Task R4 — Adopt named constructors in the tutorial

Task 8a of `docs/qq-grammar-rewrites-plan.md` landed: an alternative may
carry a leading label that names its AST constructor (`Expr = Add: Expr '+'
Term | Term ;` — see "Named constructors" in the top-level README). Label
the alternatives in `c.pg`/`asm.pg` and replace the positional helpers
(`mkImm`, `expValue`, `identName`, `symName` matching `Ctr__X__N`) with the
named constructors — that removes the tutorial's main remaining ergonomic
wart and the "positions are the first field" footnote becomes mostly
invisible. Pure rename, no behavior change; all suites stay green. Cheap
to do before C2 so later stages are written against stable names.

## Task R5 — Extract the tutorial into its own repository

Deliberately last: the directory is self-contained except for tooling via
`cabal exec`. Extraction needs: its own `.cabal` (depends on `base, array,
syb, containers, template-haskell`; alex/happy as build-tool-depends), a
way to obtain `rtk` (git dependency or a vendored binary/`cabal install`
instructions — rtk is not on Hackage), its own CI (build RTK, generate,
test, official suite), and a pointer README left behind in `tutorials/`.
Decide whether to preserve history (`git filter-repo`) or start fresh.
Blocked on the owner's call for timing; everything before this keeps the
in-repo layout working.

## Task R6 — The companion tutorial: Sandler's series, retold with RTK

### TL;DR

Write the page(s) that mimic the original blog series but teach building
the same compiler with RTK — for each blog part, what you *don't* have to
write (lexer, parser, AST types, the AST-walking boilerplate) and what you
write instead (grammar rules, QQ patterns, splices). The working code under
`tutorials/c-compiler/` is the source of truth; the page narrates it. This
doubles as RTK's missing user-facing tutorial: a reader who finishes it has
seen lexical rules, syntax rules, shortcuts, quoters, antiquote patterns,
splices, list splices, and a second grammar as an output language — all on
a real program.

### Shape

- `tutorials/c-compiler/tutorial/` with one page per stage, mirroring the
  blog's one-post-per-part structure: `00-setup.md` (toolchain, building
  RTK, the compiler-driver contract and test suite), `01-integers.md`
  (stage 1), then `02-unary.md` … `10-globals.md` as the C tasks land.
  An index in the directory plus a link from the tutorial README. Inside
  the extractable directory on purpose (task R5 takes it along).
- Each page opens with a link to the corresponding blog part and assumes
  the reader has it (or the book) at hand: the page teaches the *RTK
  delta*, not compilers from scratch.

### Content rules

- Every snippet comes from (or is verified against) the live code —
  `c.pg`/`asm.pg` rules, `Codegen.hs` QQ patterns, `Emit.hs`, real test
  invocations with real output. When a stage changes earlier code, the
  page shows the diff the way the blog does ("our grammar grows by...").
- Side-by-side moments at each blog/RTK fork: the blog's hand-written
  lexer table vs the lexical-rules section of c.pg; the blog's AST data
  declarations vs "rtk generated these, here's what they look like"; the
  blog's `generate_exp` string concatenation vs the `[asmItems| ... |]`
  splice.
- Be honest where RTK costs something: the conventions and limitations
  already catalogued in `tutorials/c-compiler/README.md` (antiquote
  shapes, token payloads, `$sym :` spacing, Prelude collisions) appear in
  the page at the moment the reader would trip over them, not as an
  appendix.

### Acceptance

- `00-setup.md` + `01-integers.md` cover everything currently
  implemented: a newcomer with a Haskell toolchain can go from cloning the
  repo to passing official stage 1 using only the pages.
- Snippets match the checked-in code at the commit that lands them
  (reviewed per stage; the per-stage checklist at the top of this plan
  keeps later stages extending the tutorial).
- The tutorial README links the pages; `tutorials/README.md` mentions them.

## Task R7 — A pretty-printer line-break option for unbracketed lists (RTK core)

### TL;DR

RTK's generated pretty-printer (`--generate-pp`) cannot stand in for the
tutorial's hand-written `Emit.hs`, but it comes much closer than it looks, and
one small, generally-useful feature would close the gap. This task adds that
feature.

### What we found (measured, not assumed)

`Emit.hs` renders the assembly AST to gas-valid AT&T text. The obvious
question is whether the generated `AsmPP` (`ppAsm :: Asm -> String`) could
replace it. Today it cannot, for one reason:

- `ppAsm` guarantees `parseAsm (ppAsm a) == a` — a round trip through *RTK's
  own* assembly parser, which (by design) treats newlines as insignificant
  whitespace. So the printer renders the whole program on one line:
  `.globl main main : movl $ 2 , %eax ret …`. gas is line-oriented and rejects
  it ("junk at end of line").

The decisive measurement: gas **tolerates everything except the missing line
breaks**. `main :` (space before the label colon) and `$ 2` (space after the
immediate sigil) both assemble and run correctly once each instruction is on
its own line. So the one fatal gap is inter-element line breaks.

The reason the breaks are missing is specific and fixable. The `block` layout
*does* break list elements onto separate lines — but only when the list is
enclosed in bracket tokens (it emits `PpOpen`/`PpBreak`/`PpClose` for a
`'{' StatementList '}'`, which is how the C grammar's body indents). Assembly's
instructions are a top-level `AsmItems = AsmItem*` with no enclosing brackets,
so the layout renders them with `intercalate [PpTok ""]` — spaces, no breaks.

### The feature

Let a list rule opt into line-broken layout regardless of brackets — e.g. a
`@layout(lines)` annotation:

```
@layout(lines)
AsmItems = AsmItem* ;
```

so the block-mode printer emits `intercalate [PpBreak]` (and, if wanted,
wraps the list in `PpOpen`/`PpClose` for indentation) for that rule. This is
not asm-specific: any language with a top-level sequence of statements or
declarations that is *not* brace-enclosed (assembly, a Makefile, a flat
script) wants exactly this. Scope it in `GenPP.hs`; add a golden and a
round-trip case; the `--pp-layout=block` machinery (`PpBreak` etc.) already
exists, so this is wiring an annotation through to it.

### Acceptance

- `asm.pg`'s `AsmItems` marked `@layout(lines)` makes `ppAsm` emit one
  instruction per line; the result assembles with gcc and runs (a stage-1
  program still exits with the right code).
- The semantic round trip `parseAsm (ppAsm a) == a` still holds.
- Goldens/round-trip tests cover the new annotation; the bracket-enclosed
  case (C's `StatementList`) is unchanged.

### Not in scope (deliberately)

Even with line breaks, `ppAsm` output stays *ugly* — `main :` not `main:`,
`$ 2` not `$2`, no instruction indentation. Making it *idiomatic* would need
token-glue control (no space before `:`, none after `$`) and an
indent-without-brackets rule. That is a much larger PP surface for cosmetic
gain, and for a *compiler* the assembly is intermediate (gcc deletes it), so
ugly-but-valid is enough. The tutorial may still prefer `Emit.hs` for the
pretty output it *shows* on the page; the point of R7 is to make the generated
printer a viable choice, not necessarily the chosen one. (See the "Why not the
generated pretty-printer?" note in `01-integers.md`.)

---

Deferred idea (not scheduled): a TACKY-style IR as a third RTK grammar
between C and assembly, following the book rather than the blog — worth
revisiting after C10, when the direct C→asm translation starts straining.
