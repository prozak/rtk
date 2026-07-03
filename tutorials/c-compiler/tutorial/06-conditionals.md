# 06 — if/else and the conditional expression

← [05 — Local variables](05-variables.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 6](https://norasandler.com/2018/02/25/Write-a-Compiler-6.html)**.

Stage 6 adds the first *control flow* in the source language: `if`/`else`
statements and the ternary `?:` expression. The code generation is a reprise —
the same compare-and-jump the stage-4 short-circuits used, now shaped into a
diamond. The parsing is where the new ideas live: the grammar meets its first
**intentional LALR conflict** (the dangling else), and the blog's
statement/declaration split quietly flips a quasi-quotation shape in our
favour.

## 1. Statements are not declarations

> **Blog ⇄ RTK.** Part 6 splits `Declaration` out of `Statement` and makes the
> function body a list of *block items*. Same split here — and it is what makes
> `if (5) int i = 0;` a **syntax** error, as the official suite requires.

An `if` branch is a statement, and C says a declaration is not one — you
cannot conditionally declare a variable. The blog encodes that in the AST;
[`c.pg`](../c.pg) encodes it in three rules:

```
Function = Func: 'int' Ident '(' ')' '{' BlockItemList '}' ;

@shortcuts(stmts)
BlockItemList = BlockItem* ;

BlockItem = Stmt: Statement
          | Decl: Declaration ;

@shortcuts(d)
Declaration = DeclInit: 'int' Ident '=' Exp ';'
            | Declare: 'int' Ident ';' ;

@shortcuts(s)
Statement = Return: 'return' Exp ';'
          | ExpStmt: Exp ';'
          | If: 'if' '(' Exp ')' Statement
          | IfElse: 'if' '(' Exp ')' Statement 'else' Statement ;
```

Declarations are only reachable through `BlockItem`, so
`if (5) int i = 0;` fails in the *parser* (the official stage-6
`declare_statement` test), not in a later pass. LALR-wise the split is free:
`int` anchors a declaration, and no statement starts with it.

There is a second, RTK-specific payoff. RTK gives each AST type **one
antiquote shape** — list if the type appears under a `*` rule, scalar
otherwise (see the README's
[known limitations](../README.md#known-rtk-limitations-to-design-around)).
Through stage 5, `Statement` was the list type (`$stmts` bound the whole
body). Now `BlockItem` holds the list and `Statement` drops to scalar shape —
exactly what if/else needs: `$s1` and `$s2` bind one branch each in the
codegen patterns below.

## 2. The dangling else — a conflict on purpose

> **Blog ⇄ RTK.** The blog's recursive-descent parser gets the dangling else
> right by construction (it parses the optional `else` greedily). An LALR
> parser gets it right by *resolving a conflict* — the interesting part is
> knowing that, pinning it, and not letting it hide anything else.

`If` and `IfElse` are separate alternatives rather than one rule with an
inline `('else' Statement)?` option. An extracted option would synthesize a
rule whose empty/present alternatives cannot carry constructor labels, putting
the passes back to matching positional `Ctr__*` names — the thing named
constructors exist to avoid.

The price is a genuine shift/reduce conflict. After

```
if ( Exp ) Statement .        -- with 'else' as the next token
```

the parser can **shift** the `else` (extend *this* if into an `IfElse`) or
**reduce** (close it as an `If`, handing the `else` to an enclosing if).
happy resolves shift/reduce in favour of shift, so the else binds to the
**nearest** if — precisely the C rule. `if (a) if (b) s1 else s2` parses as
`If a (IfElse b s1 s2)`, and the official `if_nested_*` tests all hinge on it.
(The Java grammar documents the same resolution as its conflict family 1; see
the inventory at the top of `test-grammars/java.pg`.)

A resolved conflict you rely on is a liability if new conflicts can hide
beside it, so the Makefile pins the inventory — `make conflict-check` fails
the moment the counts move:

```
conflict-check: dangling else + QQ dummy bracket, as pinned.
```

(the second being the one-per-grammar quasi-quoter bootstrap conflict that
asm.pg has carried since milestone 0).

## 3. The ternary slots into the cascade

> **Blog ⇄ RTK.** The blog inserts `<conditional-exp>` between assignment and
> logical-or. One rule here, with the same subtle asymmetry.

```
@shortcuts(e)
Exp = Assign: Ident '=' Exp
    | ,CondExp ;

Exp: CondExp = Cond: LOrExp '?' Exp ':' CondExp
             | ,LOrExp ;
```

Two choices worth noticing, both straight from the blog's grammar:

- **The false branch is `CondExp`, not `Exp`** — right-recursion, so
  `a ? 1 : b ? 2 : 3` associates as `a ? 1 : (b ? 2 : 3)`, and assignment is
  out of reach there: `flag ? a = 1 : a = 0` is a syntax error (the official
  `ternary_assign` test), while the parenthesized `(a = 0)` still works
  because parentheses re-enter the cascade at `Factor`.
- **The true branch is a full `Exp`** — `flag ? a = 1 : 0` assigns. The `?`
  and `:` bracket the middle like parentheses, so no ambiguity arises.

As always with the cascade, the precedence is baked into the tree:
`1 || 0 ? 2 : 3` parses as `Cond (Or 1 0) 2 3` with no precedence table
anywhere.

## 4. Codegen: the jump diamond, twice

> **Blog ⇄ RTK.** Compare, jump on false, run one branch, jump over the other.
> The statement form and the expression form are the *same* diamond — the only
> difference is that the expression's branches each leave a value in `%eax`.

The if/else patterns are the first users of `Statement`'s new scalar shape
([`Codegen.hs`](../Codegen.hs)):

```haskell
genStatement [statement| if ( $e ) $s1 else $s2 |] = do
  n <- fresh
  let alt = mkSym ("_if_else_" ++ show n)
      end = mkSym ("_if_end_"  ++ show n)
  c <- genExp e
  t <- genStatement s1
  f <- genStatement s2
  return $ c ++ [asmItems| cmpl $0, %eax |]
             ++ [jeTo alt] ++ t ++ [jmpTo end, label alt] ++ f ++ [label end]
```

The plain `if` drops the middle of the diamond (`jeTo end`), and the ternary
is the identical shape over `genExp` — `t` and `f` are expression code, so
whichever side runs leaves the diamond's *value* in `%eax`. All three reuse
the stage-4 label supply (`fresh`); nothing new happens in
[`asm.pg`](../asm.pg) or [`Emit.hs`](../Emit.hs) — `cmpl`, `je`, `jmp` and
labels have been there since stage 4.

[`Resolve.hs`](../Resolve.hs) barely notices the stage: declarations still
only occur in the top-level item list (the grammar guarantees it), and
`checkUses` was already a whole-subtree SYB query, so variables referenced
under an `if` or a `?:` were covered before the constructs existed.

## 5. Run it

```bash
make build
printf 'int main() { int a = 1; return a ? 2 : 3; }\n' > p.c
./ncc p.c && ./p; echo "exit: $?"
```

```
exit: 2
```

The diamond is visible in the emitted assembly:

```asm
    movl    -4(%rbp), %eax     # a
    cmpl    $0, %eax
    je      _cond_else_0
    movl    $2, %eax           # true branch
    jmp     _cond_end_0
_cond_else_0:
    movl    $3, %eax           # false branch
_cond_end_0:
    movq    %rbp, %rsp         # epilogue (return)
    pop     %rbp
    ret
```

## 6. Test it

```bash
make test                              # 103 checks, incl. conflict-check
/tmp/wacc/test_compiler.sh "$PWD/ncc" 6
```

```
PASS  dangling else binds to the NEAREST if
PASS  ternary is right-associative: [exp| 1 ? 2 : 3 ? 4 : 5 |]
PASS  declaration as an if branch is a SYNTAX error
PASS  tests/valid/nested_else.c (exit code 1)
...
===================Stage 6 Summary=================
24 successes, 0 failures
```

Stages 1–5 stay green (12, 11, 16, 27, 17) — 107/107 across the official
suite.

## What changed from stage 5

| | Stage 5 | Stage 6 |
|---|---|---|
| `c.pg` | declarations as statements | **statement/declaration split** (`BlockItem`); `If`/`IfElse`; `CondExp` |
| conflicts | none in c.pg | **1, intentional** (dangling else) — pinned by `make conflict-check` |
| `Resolve.hs` | walks `[Statement]` | walks `[BlockItem]`; nested uses already covered by SYB |
| `Codegen.hs` | frame + load/store | jump diamonds: `genStatement` if/else (scalar `$s1`/`$s2` patterns), `genCond` |
| `asm.pg` / `Emit.hs` | — | unchanged — stage 4's jumps suffice |

The structural step was in the *grammar*, not the code generator: a
deliberate, documented, pinned parsing conflict — and a type changing
antiquote shape as a side effect of where it sits in the rules.

## Next

Stage 7 adds compound statements (`{ ... }` as a statement) and real block
scoping — the resolve pass grows a scope stack, and shadowing gets its
semantics. It is task **C7** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).
