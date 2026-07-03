# 07 — Compound statements and scoping

← [06 — if/else and ?:](06-conditionals.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 7](https://norasandler.com/2018/03/14/Write-a-Compiler-7.html)**.

Stage 7 adds compound statements — `{ ... }` as a statement — and with them
real block scoping: declarations local to their block, shadowing, and
out-of-scope uses rejected. The grammar change is one line. Everything
interesting happens in the semantic pass, which meets the classic question of
separated compiler passes: **once two variables can share a name, what do you
hand the code generator?**

## 1. One grammar line

> **Blog ⇄ RTK.** The blog adds `Compound(block_item list)` to its statement
> AST. Here that is one alternative — stage 6 already built everything it
> needs.

```
Statement = ...
          | Compound: '{' BlockItemList '}' ;
```

`BlockItemList` and the statement/declaration split arrived in
[stage 6](06-conditionals.md), so a block body is *the same sort* as a
function body. LALR-wise the alternative is free — no statement starts with
`{`, so `make conflict-check` still reports exactly the pinned inventory
(dangling else + QQ dummy bracket). The official `syntax_err_extra_brace` /
`syntax_err_missing_brace` tests fall out of brace balancing in the grammar.

## 2. Scoping breaks the pass boundary — renaming fixes it

> **Blog ⇄ RTK.** This is the page's real fork. The blog resolves scopes
> *during code generation* — its codegen walks blocks carrying a variable map.
> This compiler keeps resolve and codegen separate, and shadowing is exactly
> what breaks their interface: `resolve` used to return a `name → offset` map,
> but with two live `a`s a *name* no longer identifies a slot.

The classic fix (and a first taste of what real compilers do on the way to
SSA) is **alpha-renaming**: [`Resolve.hs`](../Resolve.hs) walks the tree with
a stack of scopes, gives each declaration a unique name — `a#0`, `a#1`, … (`#`
cannot appear in a C identifier, so no collision is possible) — and rewrites
every use to the unique name of the declaration it refers to. It returns the
**renamed tree** along with offsets keyed by unique names:

```haskell
resolve :: Program -> Either String (VarMap, Program)
```

After renaming, shadowing is *gone* — every variable in the tree is distinct —
so [`Codegen.hs`](../Codegen.hs) keeps its naive map lookup untouched and its
only new case is a sequence:

```haskell
genStatement [statement| { $stmts } |] =
  concat <$> mapM genBlockItem stmts
```

The scope rules live in one small function. A scope is a `source name →
unique name` map; the stack grows at `{` and pops with the recursion; a
declaration extends the *current* scope and is an error only if that same
scope already has the name:

```haskell
declare ctx (scope : outer) (Name p v)
  | v `M.member` scope = Left ("duplicate declaration of variable '" ++ v ++ "'")
  | otherwise          = ...  -- fresh unique name, next slot, extend scope
```

Threading the scope left to right through the item list gets the official
suite's trickiest case right for free. In `declare_late`:

```c
int a = 2;
{
    a = 3;          // the OUTER a: the inner one is not declared yet
    int a = 0;
}
return a;           // 3
```

the `a = 3;` is renamed *before* the walk reaches `int a = 0;`, so it
resolves to the outer `a#0` — declarations take effect mid-block, exactly
where they stand.

## 3. The walk, structured by what the tree allows

The pass is a hand recursion over statements — but only because scopes and
declarations live there. Two shapes of traversal split the work:

- **`BlockItem`/`Statement` level — hand recursion, QQ patterns.**
  Declarations occur only in item lists; scopes open only at `{`. A handful
  of cases (`return`/expression/if/if-else/compound), each destructured and
  rebuilt with quasi-quotes — the compound case is just
  `renameItems ctx (M.empty : scopes) stmts` with the popped result rebuilt
  as `[statement| { $stmts2 } |]`.
- **`Exp` level — one generic transform.** An expression can contain *uses*
  but never declarations, so renaming inside one is scope-blind: a single
  monadic SYB pass in the `Either` monad, failing on the first unresolvable
  name:

```haskell
renameUses scopes = everywhereM (mkM rename)
  where rename (Name p v) = case lookupScopes v scopes of
          Just u  -> Right (Name p u)
          Nothing -> Left ("undeclared variable '" ++ v ++ "'")
```

Stage 5 introduced the division of labour as *QQ for targeted matching, SYB
for whole-tree queries*; stage 7 upgrades the SYB half from a query
(`listify`) to a **rewrite** (`everywhereM`) — the same `everywhere`/`mkT`
machinery the RTK rewrite recipe (task 8d) ships for every generated grammar,
here in its monadic form because the rewrite can fail.

Offsets keep growing monotonically across the whole function (the blog's
scheme): sibling blocks do not reuse slots, the frame is simply sized for
every declaration that ever lives. `frameSize` did not change.

## 4. Run it

```bash
make build
./ncc tests/valid/shadow.c && tests/valid/shadow; echo "exit: $?"
```

```
exit: 3
```

(the outer `a` — assigned `3` *before* the shadow appears — survives the
block; the inner `a` lives and dies at `-8(%rbp)` while the outer keeps
`-4(%rbp)`).

## 5. Test it

```bash
make test                              # 112 checks
/tmp/wacc/test_compiler.sh "$PWD/ncc" 7
```

```
PASS  compound pattern binds its item list: [statement| { $stmts } |]
PASS  shadowing resolves; same-scope redeclaration does not
PASS  full pipeline with blocks and shadowing (stage 7 renaming)
PASS  tests/valid/shadow.c (exit code 3)
...
===================Stage 7 Summary=================
12 successes, 0 failures
```

All previous stages stay green — **119/119** across the official suite for
stages 1–7.

## What changed from stage 6

| | Stage 6 | Stage 7 |
|---|---|---|
| `c.pg` | statement/declaration split | `+ Compound` — one alternative |
| conflicts | 1, pinned | unchanged (`{` starts nothing else) |
| **`Resolve.hs`** | flat map, walks `[BlockItem]` | **scope stack + alpha-renaming; returns the renamed tree** |
| `Codegen.hs` | jump diamonds | `+ Compound` = sequence; lookup untouched (renamed tree) |
| `Main.hs` | `resolve` → map | `resolve` → (map, renamed tree) |
| `asm.pg` / `Emit.hs` | — | unchanged |

The structural step: the first pass that *rewrites* the tree rather than just
checking it — and the pass boundary (`resolve` hands codegen a tree in which
scoping has already been compiled away) is a miniature of how real compilers
lower scoped source into flat IR.

## Next

Stage 8 adds loops — `for`, `while`, `do`, and `break`/`continue`, which need
a *loop context* threaded through statement codegen for their jump targets.
It is task **C8** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md).
