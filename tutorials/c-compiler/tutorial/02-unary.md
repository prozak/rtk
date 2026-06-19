# 02 — Unary operators

← [01 — Integers](01-integers.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 2](https://norasandler.com/2017/12/05/Write-a-Compiler-2.html)**.

Stage 2 adds the three prefix unary operators — negation `-`, bitwise
complement `~`, and logical negation `!` — and lets them nest, so
`return !-3;` and `return -~0;` compile. This is where code generation first
becomes *recursive*. The changes are small and local: the grammar grows by
one sort, codegen grows by one recursive function, and the assembly grammar
grows by four instructions.

## 1. The grammar grows by one sort

> **Blog ⇄ RTK.** Part 2 extends the lexer with three operator tokens, the AST
> with a `UnaryOp` node, and the parser with a case for it. Here that is one
> edit to [`c.pg`](../c.pg) — the token, the AST, and the parse rule are again
> one thing.

`Exp` gains a recursive alternative, and the operators get their own sort:

```
@shortcuts(e)
Exp = Unary: UnaryOp Exp
    | IntLit: intLit ;

@shortcuts(op)
UnaryOp = Neg: '-'
        | Complement: '~'
        | Not: '!' ;
```

Giving each operator its own alternative (rather than inlining `'-' Exp | '~'
Exp | …` into `Exp`) means each becomes one named constructor, so codegen can
match the *shape* `UnaryOp Exp` once and dispatch on the operator separately.
RTK derives:

```haskell
data Exp     = Anti_Exp String     | Unary RtkPos UnaryOp Exp | IntLit RtkPos Int
data UnaryOp = Anti_UnaryOp String | Neg RtkPos | Complement RtkPos | Not RtkPos
```

`Unary` is recursive (`Unary RtkPos UnaryOp Exp`), which is what lets
`!-3` parse as `Unary Not (Unary Neg (IntLit 3))`.

> **A grammar choice with teeth.** `-` here is *prefix only*: `Exp` can start
> with a `UnaryOp` or be an `intLit`, but nothing makes `4-` legal — once `4`
> is parsed as a complete `Exp`, a trailing `-` has nowhere to go, and the
> parser rejects it. That is exactly what the official suite's `wrong_order.c`
> (`return 4-;`) expects. Binary `-` arrives in stage 3, in a different
> position in the grammar.

`--analyze-conflicts` stays quiet: the new rule is right-recursive, so it adds
no LALR conflicts.

## 2. Code generation becomes recursive

> **Blog ⇄ RTK.** Part 2's `generate_expression` recurses: emit the operand,
> then emit the instruction for the operator. The RTK version is the same
> shape, but the recursion is driven by a quasi-quote pattern.

Stage 1's `genStatement` handled only `return <int>`. Now it defers to a
recursive `genExp` that leaves the expression's value in `%eax`:

```haskell
genStatement [statement| return $e ; |] = genExp e ++ [asmItems| ret |]

-- Evaluate an expression, leaving its value in %eax.
genExp :: Exp -> [AsmItem]
genExp [exp| $op1 $e1 |] = genExp e1 ++ genUnaryOp op1
genExp (IntLit _ n) = [asmItems| movl $src, %eax |]
  where src = mkImm n
```

The first pattern, `[exp| $op1 $e1 |]`, matches a `Unary` node and binds
`op1 :: UnaryOp` and `e1 :: Exp` — two adjacent antiquotes standing for the
operator and its operand. It generates the operand first (`genExp e1`,
recursing into any nesting), then the operator's instruction. For `-~0` that
unfolds to: load `0`, complement, negate.

The operators themselves are payload-free leaves, so they are matched by their
**named constructors** rather than quasi-quotes — quasi-quotes earn their keep
on *structured* nodes like `Unary`, while a one-token operator is clearest as
`Neg _`:

```haskell
-- Apply a unary operator to the value already in %eax.
genUnaryOp :: UnaryOp -> [AsmItem]
genUnaryOp (Neg _)        = [asmItems| negl %eax |]
genUnaryOp (Complement _) = [asmItems| notl %eax |]
-- logical not: set %eax to 1 if the value was 0, else 0
genUnaryOp (Not _)        = [asmItems|
                              cmpl $0, %eax
                              movl $0, %eax
                              sete %al
                            |]
```

`negl` and `notl` are one instruction each. Logical `!` is the blog's
three-instruction idiom: compare to zero (setting the zero flag), clear
`%eax` without disturbing flags, then `sete %al` writes 1 into the low byte if
the value had been zero. The `$0` in `cmpl $0, %eax` is a literal immediate,
not an antiquote — immediates are numeric and antiquotes start with a letter,
so the assembly lexer keeps them apart.

## 3. The assembly grammar grows to match

> **Blog ⇄ RTK.** The blog just prints the new mnemonics. Because our assembly
> is itself a parsed AST, the four new instructions and the byte register
> `%al` are four new alternatives in [`asm.pg`](../asm.pg).

```
AsmItem = Globl: '.globl' AsmId
        | Label: AsmId ':'
        | Movl: 'movl' Operand ',' Operand
        | Negl: 'negl' Operand
        | Notl: 'notl' Operand
        | Cmpl: 'cmpl' Operand ',' Operand
        | Sete: 'sete' Operand
        | Ret: 'ret' ;

Reg = Eax: '%eax'
    | Al: '%al' ;
```

[`Emit.hs`](../Emit.hs) gains one line per instruction and one for the new
register:

```haskell
emitItem [asmItem| negl $dst |] = "    negl    " ++ emitOperand dst
emitItem [asmItem| notl $dst |] = "    notl    " ++ emitOperand dst
emitItem [asmItem| cmpl $src, $dst |] =
  "    cmpl    " ++ emitOperand src ++ ", " ++ emitOperand dst
emitItem [asmItem| sete $dst |] = "    sete    " ++ emitOperand dst
...
emitOperand [operand| %al |] = "%al"
```

The round-trip test (`parseAsm (emit a) == a`) now covers the new instructions
for free: any of them that emitted text the assembly parser couldn't read back
would fail a test.

## 4. Run it

```bash
make build
printf 'int main() { return -5; }\n' > neg.c
./ncc neg.c && ./neg; echo "exit: $?"     # -5 & 0xFF
```

```
exit: 251
```

The assembly for `return -5;` — the operand loaded, then negated:

```asm
    .globl main
main:
    movl    $5, %eax
    negl    %eax
    ret
    movl    $0, %eax
    ret
```

and for `return !5;`, the compare/clear/set idiom:

```asm
    .globl main
main:
    movl    $5, %eax
    cmpl    $0, %eax
    movl    $0, %eax
    sete    %al
    ret
    movl    $0, %eax
    ret
```

## 5. Test it

The tutorial suite now stands at 39 checks — the stage-1 set plus four
quasi-quotation cases for unary construction/pattern matching and six
compiler programs:

```bash
make test
```

```
PASS  unary construction: [exp| -5 |]
PASS  unary pattern with antiquote binders: [exp| $op1 $e1 |]
PASS  nested unary construction: [exp| !-3 |]
PASS  tests/valid/nested_unary.c (exit code 1)
...
All stage-1 compiler tests passed.
```

And the acceptance test — stage 2 passes, and stage 1 stays green:

```bash
/tmp/wacc/test_compiler.sh "$PWD/ncc" 1
/tmp/wacc/test_compiler.sh "$PWD/ncc" 2
```

```
===================Stage 1 Summary=================
12 successes, 0 failures
===================Stage 2 Summary=================
11 successes, 0 failures
```

## What changed from stage 1

| | Stage 1 | Stage 2 |
|---|---|---|
| `c.pg` | `Exp = intLit` | `+ Unary: UnaryOp Exp`, `+ UnaryOp` sort |
| `asm.pg` | `movl`, `ret`, `%eax` | `+ negl notl cmpl sete`, `+ %al` |
| `Codegen.hs` | flat `genStatement` | recursive `genExp` + `genUnaryOp` |
| `Emit.hs` | 4 item kinds | 8 item kinds, `+ %al` operand |

The grammar carried most of the change; the passes grew by two small,
total functions.

## Next

Stage 3 adds the binary operators `+ - * /` and, with them, the expression
**precedence cascade** — the first place the `,`-lifted grammar style the
[project README](../README.md) keeps mentioning actually matters. It is task
**C3** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).
