# 04 — Relational and logical operators

← [03 — Binary operators](03-binary.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 4](https://norasandler.com/2018/01/08/Write-a-Compiler-4.html)**.

Stage 4 adds the comparisons `== != < <= > >=` and the logical connectives
`&& ||`. The comparisons slot into the cascade like any other operator, but
`&&` and `||` **short-circuit** — `a && b` must not evaluate `b` when `a` is
false — and that is the stage where code generation first becomes *stateful*:
short-circuiting needs conditional jumps, and jumps need unique labels.

## 1. Four more cascade levels

> **Blog ⇄ RTK.** The blog extends its hand-written parser with four new
> precedence tiers. Here it is four more rules on the existing cascade, and
> the AST type and parser regenerate.

C's precedence puts these *below* arithmetic: `||` (loosest), then `&&`, then
equality, then relational, then the `+`/`-` level from stage 3. So the cascade
grows four new levels on top, in [`c.pg`](../c.pg):

```
@shortcuts(e)
Exp = Or: Exp '||' LAndExp | ,LAndExp ;
Exp: LAndExp = And: LAndExp '&&' EqExp | ,EqExp ;
Exp: EqExp = Eq: EqExp EqOp RelExp | ,RelExp ;
Exp: RelExp = Rel: RelExp RelOp AddExp | ,AddExp ;
Exp: AddExp = Add: AddExp AddOp MulExp | ,MulExp ;   -- (stage 3, now renamed)
...
@shortcuts(eqop)
EqOp = Equal: '==' | NotEqual: '!=' ;
@shortcuts(relop)
RelOp = Lt: '<' | Le: '<=' | Gt: '>' | Ge: '>=' ;
```

Two modelling choices:

- **`&&` and `||` get their own constructors (`And`/`Or`), not an operator
  sort.** The comparison levels factor the operator out (`Eq EqExp EqOp
  RelExp`) because `==` and `!=` are operators over two computed values. But
  `&&`/`||` are *control flow* — they decide whether to evaluate their right
  operand at all — so they are distinct constructors with no operator to
  carry. They take a literal `'||'`/`'&&'` token.
- **The shared `-` problem returns as the shared `!`.** `!` is unary `Not`
  (stage 2) and `!=` is `NotEqual`. They overlap exactly the way binary and
  unary `-` do; alex's maximal munch takes the longer `!=` when both
  characters are present, so `!x` and `x != y` both tokenize correctly. (A
  `TestQQ` case pins this: `[exp| 1 != 2 |]` must match `$eqop` as `!=`, not
  `!` then `=`.)

The one `Exp` type now carries one constructor per level:

```haskell
data Exp = ... | Add RtkPos Exp AddOp Exp
               | Rel RtkPos Exp RelOp Exp
               | Eq  RtkPos Exp EqOp Exp
               | And RtkPos Exp Exp     -- control flow: no operator field
               | Or  RtkPos Exp Exp
```

## 2. Code generation becomes stateful

> **Blog ⇄ RTK.** Same idea as the blog — emit a `cmp`/`set` for comparisons,
> and conditional jumps around unique labels for short-circuiting. The blog
> threads a label counter by hand; here it is a `State Int`.

Comparisons are pure: compare, then turn a condition flag into a `0`/`1` in
`%eax`. With the left operand in `%eax` and the right in `%ecx` (from the
stage-3 `genBinary`), one helper covers all six:

```haskell
compareSet :: [AsmItem] -> [AsmItem]
compareSet setcc = [asmItems| cmpl %ecx, %eax
                              movl $0, %eax |] ++ setcc   -- mov preserves the flags

applyRelOp (Lt _) = compareSet [asmItems| setl %al |]
applyRelOp (Le _) = compareSet [asmItems| setle %al |]
applyEqOp (Equal _) = compareSet [asmItems| sete %al |]
...
```

Short-circuiting is where state arrives. `a && b` becomes: evaluate `a`; if it
is zero, the answer is zero and `b` is skipped; otherwise the answer is
`b != 0`. The jump targets must be **unique** across the whole function, so
generation runs in a counter monad:

```haskell
type Gen = State Int
fresh :: Gen Int
fresh = state (\n -> (n, n + 1))

codegen :: Program -> Asm
codegen prog = evalState (genProgram prog) 0
```

Adding the monad rippled through every generator that can reach a label —
`genProgram`, `genStatement`, `genExp`, `genBinary` all became `Gen`-returning
(the pure `apply*`/`genUnaryOp` helpers did not). With the counter in hand,
`genAnd` lays out the jumps:

```haskell
genAnd e1 e2 = do
  n <- fresh
  let rhs = mkSym ("_and_rhs_" ++ show n)
      end = mkSym ("_and_end_" ++ show n)
  l <- genExp e1
  r <- genExp e2
  return $ l
    ++ [asmItems| cmpl $0, %eax |]
    ++ [jneTo rhs, jmpTo end, label rhs]   -- a != 0 -> rhs; else fall to end, %eax = 0
    ++ r
    ++ [asmItems| cmpl $0, %eax
                  movl $0, %eax
                  setne %al |]
    ++ [label end]
```

The jumps and the label definition each carry just a label *name*, so — like
the integer and identifier leaves back in stage 1 — they go through small
constructor helpers (`jneTo`, `jmpTo`, `label`), and the name itself is built
with `mkSym`, since a token payload can't be spliced by an antiquote. `genOr`
is the mirror image (jump to the right operand when the left is *zero*,
otherwise load `1`).

## 3. The assembly grammar grows

[`asm.pg`](../asm.pg) gains the `setcc` family (`setne setl setle setg setge`;
`sete` arrived in stage 2) and three jumps that target a label:

```
AsmItem = ... | Setne: 'setne' Operand | Setl: 'setl' Operand | ...
              | Je: 'je' AsmId | Jne: 'jne' AsmId | Jmp: 'jmp' AsmId | ... ;
```

`Emit.hs` gains one line each — the jumps print their `AsmId` as the bare
label name (`jne     _and_rhs_0`).

## 4. Run it

```bash
make build
printf 'int main() { return 1 < 2; }\n' > p.c
./ncc p.c && ./p; echo "exit: $?"
```

```
exit: 1
```

A comparison is `cmpl` then `setcc`:

```asm
    movl    $2, %eax
    push    %rax
    movl    $1, %eax
    pop     %rcx
    cmpl    %ecx, %eax
    movl    $0, %eax
    setl    %al
    ret
```

and `0 && 5` jumps straight past the right operand — the short circuit, made
operational:

```asm
    movl    $0, %eax
    cmpl    $0, %eax
    jne     _and_rhs_0
    jmp     _and_end_0
_and_rhs_0:
    movl    $5, %eax
    cmpl    $0, %eax
    movl    $0, %eax
    setne   %al
_and_end_0:
    ret
```

The payoff is observable: `return 1 || (1 / 0);` exits `1` instead of crashing
with a divide-by-zero, because the `1 / 0` is jumped over.

## 5. Test it

```bash
make test                              # 72 checks
/tmp/wacc/test_compiler.sh "$PWD/ncc" 4
```

```
PASS  precedence: && binds tighter than ||: [exp| 1 || 0 && 2 |]
PASS  maximal munch: != is one token (NotEqual), not ! then =
PASS  tests/valid/short_circuit_or.c (exit code 1)
PASS  tests/valid/short_circuit_and.c (exit code 0)
...
===================Stage 4 Summary=================
24 successes, 0 failures
```

(The official suite's `skip_on_failure_*` short-circuit cases use local
variables and assignment — stage 5 — so the harness does not count them yet;
they parse and pass once stage 5 lands.)

## What changed from stage 3

| | Stage 3 | Stage 4 |
|---|---|---|
| `c.pg` | cascade to `Add` | `+ Or And Eq Rel` levels, `EqOp`/`RelOp` |
| `asm.pg` | arithmetic + `push`/`pop` | `+ setcc` family, `je`/`jne`/`jmp` |
| `Codegen.hs` | pure | `State Int` label supply; `genAnd`/`genOr`, `compareSet` |
| `Emit.hs` | — | setcc + jump lines |

The grammar absorbed the new precedence levels; the real new idea on the code
side was the label supply, which every later control-flow stage (`if`, loops)
reuses.

## Next

Stage 5 adds local variables — declarations, assignment, and variable
references — which brings the compiler's first **semantic pass** (resolving
names to stack slots) and a new way to reject programs. It is task **C5** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).
