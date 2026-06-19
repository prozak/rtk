# 03 — Binary operators and the precedence cascade

← [02 — Unary operators](02-unary.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 3](https://norasandler.com/2017/12/15/Write-a-Compiler-3.html)**.

Stage 3 adds `+`, `-`, `*`, `/` — with correct precedence (`2 + 3 * 4` is
`14`, not `20`), left associativity (`10 - 3 - 4` is `3`), and parentheses.
This is the stage that shapes every later one: the expression grammar becomes
a **precedence cascade**, and code generation becomes a little stack machine.

## 1. Precedence lives in the grammar

> **Blog ⇄ RTK.** Precedence is the part of parsing the blog spends the most
> care on — it hand-writes a parser that climbs precedence levels (or threads
> precedence through recursive descent) so `*` binds tighter than `+`. With
> RTK, precedence is just the *shape* of the grammar; the parser falls out.

The expression grammar in [`c.pg`](../c.pg) is a stack of levels, lowest
precedence (additive) on top, each level falling through to the next:

```
@shortcuts(e)
Exp = Add: Exp AddOp MulExp
    | ,MulExp ;

Exp: MulExp = Mul: MulExp MulOp UnaryExp
            | ,UnaryExp ;

Exp: UnaryExp = Unary: UnaryOp UnaryExp
              | ,Factor ;

Exp: Factor = IntLit: intLit
            | '(' ,Exp ')' ;

@shortcuts(aop)
AddOp = Plus: '+' | Minus: '-' ;
@shortcuts(mop)
MulOp = Times: '*' | Divide: '/' ;
```

Three things are doing the work:

- **`Exp:` shares one type across all levels.** `Exp: MulExp = …` means the
  rule `MulExp` has data type `Exp`, and likewise `UnaryExp` and `Factor`. So
  although the grammar has four levels, the AST has a *single* `Exp` type.
- **`,` lifts the fall-through.** `Exp = Add: … | ,MulExp` says an additive
  expression is either an `Add` node *or* — via the `,`-lifted alternative —
  just a multiplicative expression, passed straight through with **no wrapper
  node**. Without the lift, every level would add a constructor (`AddOf`,
  `MulOf`, …) that codegen would have to peel; with it, a plain `2` is just
  `IntLit 2`, not `Add(Mul(Unary(Factor(IntLit 2))))`.
- **Left recursion gives left associativity.** `Exp = Add: Exp AddOp MulExp`
  recurses on the *left*, so `10 - 3 - 4` groups as `(10 - 3) - 4`. (The blog
  writes this as an EBNF loop; Happy consumes left recursion directly.)

The result is one clean type with one constructor per precedence level:

```haskell
data Exp = Anti_Exp String
         | IntLit RtkPos Int
         | Unary  RtkPos UnaryOp Exp
         | Mul    RtkPos Exp MulOp Exp
         | Add    RtkPos Exp AddOp Exp
```

Precedence is now a fact about the *tree*: `2 + 3 * 4` parses to
`Add 2 (Mul 3 4)`, and parentheses, which are a `,`-lift in `Factor`, regroup
without leaving a node behind — `(2 + 3) * 4` is `Mul (Add 2 3) 4`. The
`TestQQ` suite asserts exactly these shapes.

> **The shared `-`.** `-` is both an `AddOp` (binary) and a `UnaryOp`
> (prefix). The cascade tells them apart by position: after a complete
> expression, `-` continues the additive level; at the start of an operand it
> begins a `UnaryExp`. So `2 - -3` parses as `2 minus (negate 3)` with no
> special-casing.

> **One benign conflict.** Happy reports a single shift/reduce conflict and
> resolves it by shifting. The bare four-level cascade is conflict-free; the
> conflict comes from the quasi-quoter scaffolding RTK wraps around the start
> symbol (the dummy-token production that lets `[exp| … |]` parse any sort)
> meeting the left-recursive `Exp`. Shifting is the correct resolution —
> continue the expression — and every precedence, associativity, and
> parenthesization test passes, including the official suite's. If a QQ
> pattern ever *did* misbehave, the place to look is the attach points in the
> generated `CParser.y`.

## 2. Code generation: a tiny stack machine

> **Blog ⇄ RTK.** Same algorithm as the blog — evaluate one side, save it on
> the stack, evaluate the other, combine — driven by one QQ pattern per
> precedence level.

Because the cascade put `Add` and `Mul` in the same `Exp` type, one
quasi-quote pattern matches each, told apart by the operator's sort:

```haskell
genExp :: Exp -> [AsmItem]
genExp [exp| $e1 $aop $e2 |] = genBinary e1 e2 (applyAddOp aop)
genExp [exp| $e1 $mop $e2 |] = genBinary e1 e2 (applyMulOp mop)
genExp [exp| $op1 $e1 |]     = genExp e1 ++ genUnaryOp op1
genExp (IntLit _ n)          = [asmItems| movl $src, %eax |]
  where src = mkImm n
```

`genBinary` is the stack machine. The one subtlety is evaluation order: it
generates the **right** operand first and pushes it, then the left into
`%eax`, then pops the right into `%ecx`. Doing the right side first is what
leaves the *left* operand in `%eax`, which is where `subl` and `idivl` expect
it:

```haskell
genBinary :: Exp -> Exp -> [AsmItem] -> [AsmItem]
genBinary e1 e2 apply =
  genExp e2 ++ [asmItems| push %rax |]
            ++ genExp e1 ++ [asmItems| pop %rcx |]
            ++ apply
```

With the left operand in `%eax` and the right in `%ecx`, each operator is one
or two instructions:

```haskell
applyAddOp (Plus _)  = [asmItems| addl %ecx, %eax |]
applyAddOp (Minus _) = [asmItems| subl %ecx, %eax |]   -- eax = eax - ecx = left - right

applyMulOp (Times _)  = [asmItems| imull %ecx, %eax |]
applyMulOp (Divide _) = [asmItems| cdq           -- sign-extend %eax into %edx:%eax
                                   idivl %ecx |] -- quotient -> %eax
```

`push`/`pop` are 64-bit instructions (`%rax`/`%rcx`); the 32-bit value rides
in the low half, and a 32-bit write like `movl $5, %eax` zeroes the upper
half, so the value survives the round trip. This 32-/64-bit split is why
`Reg` in `asm.pg` now lists both `%eax`/`%ecx` and `%rax`/`%rcx`.

## 3. The assembly grammar grows

[`asm.pg`](../asm.pg) gains the arithmetic instructions, the division pair,
the stack ops, and three registers:

```
AsmItem = ... | Addl: 'addl' Operand ',' Operand
              | Subl: 'subl' Operand ',' Operand
              | Imull: 'imull' Operand ',' Operand
              | Cdq: 'cdq'
              | Idivl: 'idivl' Operand
              | Push: 'push' Operand
              | Pop: 'pop' Operand
              | ... ;

Reg = Eax: '%eax' | Al: '%al' | Ecx: '%ecx' | Rax: '%rax' | Rcx: '%rcx' ;
```

In [`Emit.hs`](../Emit.hs), the register set has outgrown one-quasi-quote-per
register, so registers are now rendered by named constructor through a small
`emitReg` — the same readability win named constructors gave the C side:

```haskell
emitOperand (Imm _ n)   = "$" ++ show n
emitOperand (RegOp _ r) = emitReg r

emitReg (Eax _) = "%eax"
emitReg (Ecx _) = "%ecx"
emitReg (Rax _) = "%rax"
...
```

## 4. Run it

```bash
make build
printf 'int main() { return 2 + 3 * 4; }\n' > p.c
./ncc p.c && ./p; echo "exit: $?"
```

```
exit: 14
```

The assembly shows the stack machine evaluating `3 * 4` before adding `2` —
precedence made operational:

```asm
    .globl main
main:
    movl    $4, %eax
    push    %rax
    movl    $3, %eax
    pop     %rcx
    imull   %ecx, %eax
    push    %rax
    movl    $2, %eax
    pop     %rcx
    addl    %ecx, %eax
    ret
    movl    $0, %eax
    ret
```

and `return 7 / 2;` uses the `cdq`/`idivl` pair (result `3`, truncated):

```asm
    movl    $2, %eax
    push    %rax
    movl    $7, %eax
    pop     %rcx
    cdq
    idivl   %ecx
    ret
```

## 5. Test it

```bash
make test                              # 54 checks
/tmp/wacc/test_compiler.sh "$PWD/ncc" 3
```

```
PASS  precedence baked into the AST: [exp| 2 + 3 * 4 |]
PASS  parentheses regroup: [exp| (2 + 3) * 4 |]
PASS  binary pattern with operator binder: [exp| $e1 $aop $e2 |]
PASS  tests/valid/associativity.c (exit code 3)
...
===================Stage 3 Summary=================
16 successes, 0 failures
```

## What changed from stage 2

| | Stage 2 | Stage 3 |
|---|---|---|
| `c.pg` | `Exp = Unary \| IntLit` | precedence cascade (`Add`/`Mul`/`Unary`/`IntLit`), `AddOp`/`MulOp` |
| `asm.pg` | `negl notl cmpl sete` | `+ addl subl imull cdq idivl push pop`, `+ %ecx %rax %rcx` |
| `Codegen.hs` | recursive `genExp` | `+ genBinary` stack machine, `applyAddOp`/`applyMulOp` |
| `Emit.hs` | inline registers | `emitReg` by named constructor, `binOperands` |

The grammar carried the hard part — precedence and associativity — and the
passes grew by a stack machine and two operator tables.

## Next

Stage 4 adds the relational and logical operators (`== != < <= > >= && ||`),
which extend the cascade further and bring short-circuit evaluation, where
code generation first needs a supply of unique labels. It is task **C4** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).
