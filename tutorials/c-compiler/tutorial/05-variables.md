# 05 — Local variables

← [04 — Relational and logical](04-relational.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 5](https://norasandler.com/2018/01/25/Write-a-Compiler-5.html)**.

Stage 5 adds local variables: declarations (`int a = 2;`), assignment
(`a = a + 3;`), and references (`return a;`). Two things are genuinely new.
The compiler grows a **stack frame** — locals live in memory, addressed off a
frame pointer — and, for the first time, it gains a **semantic pass**: a check
that runs after parsing and rejects programs the *grammar* happily accepts but
that are nonetheless wrong (an undeclared variable, a variable declared twice).

## 1. Assignment, declarations, and references

> **Blog ⇄ RTK.** The blog extends its parser with assignment expressions,
> declaration statements, and variable references. Here it is a few more rules,
> with one careful choice about where assignment sits.

Assignment is the *lowest*-precedence operator in C and is right-associative,
so it goes on top of the cascade, recursing right; declarations and bare
expressions become statements; and a variable reference is a new kind of
`Factor` ([`c.pg`](../c.pg)):

```
Statement = Return: 'return' Exp ';'
          | ExpStmt: Exp ';'
          | DeclInit: 'int' Ident '=' Exp ';'
          | Declare: 'int' Ident ';' ;

@shortcuts(e)
Exp = Assign: Ident '=' Exp | ,LOrExp ;      -- right-recursive: a = b = c is a = (b = c)
...
Exp: Factor = IntLit: intLit | VarRef: Ident | '(' ,Exp ')' ;
```

The one subtle choice is that assignment's left side is a bare `Ident`, not a
general `Exp`. That does two jobs at once:

- **It keeps the grammar LALR(1)-clean.** An identifier at the start of an
  expression is ambiguous — is `a` the target of `a = …`, or a variable
  reference? The parser resolves it with one token of lookahead: it shifts the
  `Ident`, and if `=` follows it is an assignment, otherwise the `Ident`
  reduces to a `Factor`. Because `=` never follows a `Factor` anywhere else in
  the cascade, there is no conflict — `--analyze-conflicts` and happy both stay
  quiet. (This is why `=` and `==` must be distinct tokens.)
- **It makes `a + 3 = 4` a *syntax* error, for free.** A non-identifier left
  side simply will not parse, which is exactly what the blog wants — and what
  the official suite's `syntax_err_bad_lvalue` expects.

So the `Exp` type gains `Assign RtkPos Ident Exp` and `VarRef RtkPos Ident`,
and `Statement` gains the two declaration forms.

## 2. The first semantic pass

> **Blog ⇄ RTK.** The blog walks the AST to build a variable map and reject
> bad programs. So does this — but the "find every variable used here" query is
> one generic call, not a hand-written recursion over ten expression
> constructors.

The grammar accepts `return a;` with no `a`, and `int a; int a;`. Catching
those is not the parser's job; it is a *pass* over the parsed tree.
[`Resolve.hs`](../Resolve.hs) walks the statement list in order, assigns each
declared local a stack offset, and rejects use-before-declaration and
redeclaration:

```haskell
resolveStmts = go M.empty
  where
    go env [] = Right env
    go env (s : rest) = case s of
      DeclInit _ ident e -> declare env rest (identName ident) (checkUses env e)
      Declare _ ident    -> declare env rest (identName ident) (Right ())
      _                  -> checkUses env s >> go env rest

    declare env rest v initOk =
      if v `M.member` env
        then Left  ("duplicate declaration of variable '" ++ v ++ "'")
        else initOk >> go (M.insert v (-4 * (M.size env + 1)) env) rest

    checkUses env node =
      case filter (`M.notMember` env) (referenced node) of
        []      -> Right ()
        (v : _) -> Left ("undeclared variable '" ++ v ++ "'")
```

`checkUses` needs the set of variables a statement (or an initializer)
references, *anywhere* inside it. Writing that as a recursion over `Or`/`And`/
`Add`/`Mul`/`Unary`/`Assign`/`VarRef`/… would be ten dull cases. Instead it is
one **SYB** query — `listify` over the `Data` instance RTK already derives for
every AST type:

```haskell
import Data.Generics (listify)

referenced :: Data a => a -> [String]
referenced = map nameOf . listify isName
  where isName (Name _ _) = True; isName _ = False
        nameOf (Name _ s) = s;    nameOf _ = ""
```

`listify isName` finds every `Ident` node at any depth and returns them as a
list. This is the division of labour the whole tutorial has been building
toward: **quasi-quoters for targeted construction and matching, generic
programming for whole-tree queries.** Use the right one for each job, and a
real pass stays short.

`resolve` returns `Either String VarMap`, and the driver runs it between
parsing and code generation, with the same contract as a parse error — reject,
exit non-zero, write nothing:

```haskell
asm <- case scanTokens src >>= parseC of
  Left err  -> reject err
  Right ast -> case resolve ast of
    Left err     -> reject err
    Right varmap -> return (codegen varmap ast)
```

## 3. Code generation grows a stack frame

> **Blog ⇄ RTK.** The same frame setup and the same load/store as the blog,
> with the variable map threaded by a `Reader`.

Code generation now reads the variable→offset map (from `Resolve`) and still
threads the label counter from stage 4, so it runs in
`ReaderT VarMap (State Int)`. Every function gets a prologue that saves the
caller's frame pointer, points `%rbp` at the new frame, and reserves space for
the locals; every return path runs the matching epilogue:

```haskell
prologue n = [asmItems| push %rbp
                        movq %rsp, %rbp |]
             ++ [Subq rtkNoPos (mkImm n) rspOp | n > 0]   -- carve out n bytes

epilogue   = [asmItems| movq %rbp, %rsp
                        pop %rbp
                        ret |]
```

A variable reference loads from its slot; an assignment evaluates the
right-hand side and stores it — *and leaves the value in `%eax`*, because
assignment is an expression (`int b = a = 0` works because `a = 0` returns
`0`):

```haskell
genExp [exp| $name = $e |] = do      -- assignment
  e' <- genExp e
  dst <- offsetOf name
  return (e' ++ [asmItems| movl %eax, $dst |])
genExp [exp| $name |] = do           -- reference
  src <- offsetOf name
  return [asmItems| movl $src, %eax |]
```

`offsetOf` is the only place the `Reader` is consulted — it looks the name up
in the map and builds a memory operand. The offset is an `Int` leaf, so it is
built with a constructor helper (`mkMem`), the same way immediates have been
since stage 1, not antiquoted.

## 4. The assembly grammar grows

[`asm.pg`](../asm.pg) gains 64-bit `movq`/`subq` (for the frame pointer and
stack pointer, which are 64-bit), the registers `%rbp`/`%rsp`, and a **memory
operand** — a signed displacement off a base register, AT&T `disp(base)`:

```
Operand = Imm: '$' num | RegOp: Reg | Mem: num '(' Reg ')' ;
Int: num = '-'? [0-9]+ ;      -- signed: locals are at negative offsets, -4(%rbp)
```

Making `num` signed is what lets `-4(%rbp)` lex as one displacement; the `-`
is part of the number, and since `Movl` already takes two `Operand`s, it
handles a memory source or destination with no new instruction.

## 5. Run it

```bash
make build
printf 'int main() { int a = 5; a = a + 1; return a; }\n' > p.c
./ncc p.c && ./p; echo "exit: $?"
```

```
exit: 6
```

The whole frame is visible — set up, the local stored and loaded at
`-4(%rbp)`, then torn down:

```asm
main:
    push    %rbp
    movq    %rsp, %rbp
    subq    $16, %rsp
    movl    $5, %eax
    movl    %eax, -4(%rbp)     # int a = 5
    movl    $1, %eax
    push    %rax
    movl    -4(%rbp), %eax
    pop     %rcx
    addl    %ecx, %eax         # a + 1
    movl    %eax, -4(%rbp)     # a = ...
    movl    -4(%rbp), %eax     # return a
    movq    %rbp, %rsp
    pop     %rbp
    ret
    ...
```

## 6. Test it

```bash
make test                              # 88 checks
/tmp/wacc/test_compiler.sh "$PWD/ncc" 5
```

```
PASS  assignment is right-associative: [exp| a = b = c |]
PASS  full pipeline: parse C -> resolve -> codegen -> emit -> parse Asm
PASS  tests/invalid/use_before_decl.c (rejected)
...
===================Stage 5 Summary=================
17 successes, 0 failures
```

A free bonus: stage 4's official score rose from 24 to **27**. Its
`skip_on_failure_*` short-circuit tests use local variables, so the harness
skipped them before; now they compile and pass — a second, independent check
that stage 4's short-circuit jumps are right.

## What changed from stage 4

| | Stage 4 | Stage 5 |
|---|---|---|
| `c.pg` | comparisons/logicals | `+ Assign`, `VarRef`, declaration statements |
| **`Resolve.hs`** | — | **new: the first semantic pass (SYB query + var→offset map)** |
| `asm.pg` | jumps + setcc | `+ movq subq`, `Mem` operand, `%rbp`/`%rsp`, signed `num` |
| `Codegen.hs` | `State Int` | `ReaderT VarMap (State Int)`; prologue/epilogue; load/store |
| `Main.hs` | parse → codegen | parse → **resolve** → codegen |

The structural step was the new pass — the first time the compiler rejects a
program for a reason the grammar cannot express.

## Next

Stage 6 adds `if`/`else` statements and the `?:` conditional expression — the
first *control flow* in the source language, which brings the classic
dangling-else parsing question and reuses the stage-4 label supply. It is task
**C6** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).
