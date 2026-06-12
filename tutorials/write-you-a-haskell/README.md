# Write You a Haskell on RTK

This project reimplements the languages of Stephen Diehl's tutorial
["Write You a Haskell"](https://github.com/sdiehl/write-you-a-haskell)
using RTK as the frontend toolkit. Where the tutorial hand-writes Parsec
parsers and abstract syntax per chapter, here each language is a `.pg`
grammar in this directory from which RTK generates the lexer, parser, AST
and quasi-quoters; the semantic passes (evaluation, substitution, type
checking, inference) are written by pattern-matching on *concrete syntax*
via the generated quasi-quoters - something the tutorial itself cannot do.

Like the other tutorials, this directory only reaches into the parent
checkout for the RTK toolchain. Build RTK first, then drive everything
through this directory's Makefile:

```
cd ../.. && cabal build     # once: build rtk, with alex/happy available
make test                   # all four languages' suites (110 checks)
make repl-poly              # or repl-lc / repl-stlc / repl-proto
```

## Status

| Phase | Tutorial chapters | Language | State |
|-------|-------------------|----------|-------|
| 0 | - | `lc.pg` spike: QQ viability on a ladder grammar | done |
| 1 | 3 (parsing), 4 (untyped LC) | `lc.pg` + `lc-main.hs` interpreter | done |
| 2 | 5-6 (type systems, evaluation) | `stlc.pg` + `stlc-main.hs` | done |
| 3 | 7 (Hindley-Milner) | `poly.pg` + `poly-main.hs` | done |
| 4 | 8-12 (ProtoHaskell-lite) | `proto.pg` + `proto-main.hs`, explicit `{ ; }` | done |

Phase 4 uses explicit block delimiters; making RTK able to lex layout
(the offside rule) so the same grammar accepts indented programs is
tracked independently in [issue #95](https://github.com/prozak/rtk/issues/95).

Chapter 3's content (writing a parser combinator library) is replaced by
RTK itself; its calculator example is subsumed by `lc.pg`. Chapters 13+
of the tutorial were never written.

## The lc language (chapters 3-4)

`lc.pg` defines an untyped lambda calculus with the surface
syntax of the tutorial's chapter-7 Poly language: `\x -> e` lambdas,
`let .. in`, `if/then/else`, an `==` / `+ -` / `*` operator ladder,
application by juxtaposition, integer and boolean literals, and `--`
comments. The grammar is conflict-free under happy.

`lc-main.hs` implements, entirely with QQ patterns over the
generated AST: a call-by-value evaluator with closures and environments
(the design of the tutorial's `Eval.hs`), free variables,
capture-avoiding substitution, a precedence-aware pretty-printer, and a
REPL.

```
make test-lc    # build the generated pipeline and run the test suite
make repl-lc    # interactive REPL: lc> (\s -> \z -> s (s z)) (\n -> n + 1) 0
```

## The stlc language (chapters 5-6)

`stlc.pg` extends the same expression ladder with typed
binders and a second QQ-capable nonterminal family for types. Annotated
binders use the classic dot form `\x : Int . e`: with an arrow instead,
`Int -> e` would parse as a function type under LALR. The type arrow is
right-recursive (`Int -> Int -> Bool` associates right), and `TyAtom`
plays the lift-free anti-token role for the `Ty` family that `Atom`
plays for `Expr`.

`stlc-main.hs` implements chapter 5's typechecker with
quasi-quotes over both families - result types are built by splicing
(`[ty| $t1 -> $t2 |]`) and application destructures arrows with a
`[ty| ... |]` pattern - plus chapter 6's evaluation-strategy comparison:
one closure evaluator parameterized by strategy, where call-by-value
forces arguments with `seq` and call-by-name inherits thunking from the
host language. The tests pin the classic result: the strategies disagree
exactly on an ill-typed term (`(\x : Int . 2) (1 + true)` - CBV gets
stuck, CBN returns 2), and the typechecker rejects that term.

```
make test-stlc    # typechecker + evaluator test suite
make repl-stlc    # stlc> (\f : Int -> Int . f (f 0)) (\n : Int . n + 1)
                  #       2 : Int
```

## The poly language (chapter 7)

`poly.pg` is the tutorial's ML-flavored Poly: programs are
lists of `;`-terminated declarations (`let f x y = e;`, `let rec`, bare
expressions), with multi-parameter lambdas, `let`/`let rec .. in`, `fix`
and bare binders - types are inferred, so the grammar has no type
syntax at all. Grammar notes: top-level `let x = e;` and the expression
`let x = e in e` share a prefix that LALR resolves by shifting, but only
because the declaration's parameter list cannot be empty; and lambda
parameters are a list of a dedicated wrapper type `Param`, because a
type's anti rule is cached either scalar or list and `Id` must stay
scalar for `let $x1 = ...` patterns. The only happy conflict is one
benign shift/reduce inside the generated QQ start-wrapper rule (an empty
program makes the start symbol nullable); it is unreachable from real
input.

`poly-main.hs` implements the tutorial pipeline:
desugaring as quasi-quote rewrites (currying multi-parameter lambdas,
`let rec f = e` to `let f = fix (\f -> e)`), algorithm W - unification,
occurs check, instantiate/generalize, let-polymorphism - over the
generated AST with QQ patterns, call-by-value evaluation where `fix`
ties a lazy knot through the closure environment, and a stateful REPL.

```
make test-poly    # desugaring + inference + program tests (fib 10 => 55 : Int)
make repl-poly    # poly> let rec fib n = if n == 0 then 0 else
                  #         if n == 1 then 1 else fib (n - 1) + fib (n - 2);
                  # poly> fib 10
                  #       55 : Int
                  # poly> \f g x -> f (g x)
                  #       <<closure>> ... : (a -> b) -> (c -> a) -> c -> b
```

## The proto language (chapters 8-12, lite)

`proto.pg` extends Poly with algebraic data types and
case expressions, using explicit `{ ; }` blocks where Haskell would use
layout: `data List a = Nil | Cons a (List a);` (the constructor list is
the first use of the `+ ~ sep` separated-list form) and
`case e of { Nil -> 0 ; Cons x xs -> 1 + len xs }`. Capitalized
constructors get their own token class, and the wrapper-type idiom from
poly.pg is applied throughout (`TyVar`/`Param` wrap `Id`, `Field` wraps
`Ty`, `PArg` wraps `Pat`).

`proto-main.hs` runs each declaration through the
tutorial's passes: data declarations become constructor schemes
(chapter 10), a renamer checks scope, constructor arity in patterns and
duplicate pattern variables before inference (chapter 11), desugaring
stays pure QQ rewriting, inference extends algorithm W with
parameterized `TData` types and case/pattern inference, and evaluation
treats constructors as curried values with direct nested-pattern
matching (chapter 12 lite - compilation of nested patterns to simple
case trees is left out). `map` over a user-defined list infers
`(a -> b) -> List a -> List b`.

```
make test-proto   # ADTs, renamer, case inference, nested patterns
make repl-proto   # proto> data List a = Nil | Cons a (List a);
                  # proto> let rec map0 f l = case l of { Nil -> Nil ;
                  #          Cons x xs -> Cons (f x) (map0 f xs) };
                  # proto> map0 (\n -> n * n) (Cons 1 (Cons 2 Nil))
                  #        Cons 1 (Cons 4 Nil) : List Int
```

## Grammar design rules for quasi-quotation

Distilled from the phase-0 spike (and from why QQ splicing fails for
`java.pg`):

1. **Share one AST type across the precedence ladder** with `Type: Rule`
   annotations, and collapse pass-through alternatives with `,` lifts so
   precedence leaves no wrapper constructors:
   `Expr: Add = Add '+' Mul | ,Mul ;`
2. **Keep the bottom (atom) rule free of lift-headed alternatives.**
   Normalization injects the `Anti_<Type>` quasi-quotation alternative
   into a rule with no alternative *starting* with a lift, so in a fully
   lifted ladder the anti token lands exactly once - on the atom rule -
   and antiquotes (`$e1`) become parseable at every precedence level. A
   parenthesized `'(' ,Expr ')'` alternative does not block injection
   (it starts with an ignored token). Since the unit-production-cover
   change, RTK guarantees one splice alternative per shared-type group
   even for hierarchies that violate this layout; the lifted ladder
   remains the clean way to get it at the atom level.
3. **Declare short metavariable prefixes** with `@shortcuts` (e.g.
   `@shortcuts(e)` on the `Expr` rule) so quotes can write `$e1`, `$x2`;
   lowercased type names are available as prefixes automatically.
4. **Token fields are not antiquotable** (e.g. the `Int` of a literal),
   so a consumer needs a raw constructor for exactly those cases - wrap
   them in explicitly bidirectional pattern synonyms that ignore the
   constructors' position field when matching and supply `rtkNoPos` when
   constructing (`pattern LitI n <- Ctr__Expr__0 _ n where ...`).
5. **Keywords win over the identifier rule** automatically: generated
   lexers emit string-literal tokens before lexical rules and Alex
   prefers earlier rules on equal-length matches.

## RTK fixes and findings along the way

- The `.pg` string-literal rule could not tokenize literals ending in a
  backslash (`'\\'`): the trailing backslash absorbed the closing quote
  under maximal munch. Fixed by consuming escape pairs atomically in
  both front ends - `grammar.pg`'s authoritative str rule (spelled with
  `[\\x5C]` hex escapes, since the bootstrap stage is lexed by the
  previous front end) and the hand-written reference `Lexer.x`
  (`([^'\\] | \\ .)*`). This is what previously forced `java.pg` into
  hex workarounds and the spike into a `'fn'` lambda keyword.
- Alex quoted strings are *literal* - backslash is not an escape
  character inside `"..."` (`"\"` matches one backslash, `"\'"` matches
  backslash-quote). `GenX.backquoteStr` documents this; do not "fix" it
  to double backslashes.
- Regex character classes (`[...]` in `.pg`) still pass backslashes
  through to Alex, where sets *do* process escapes - a literal backslash
  inside a class still needs the `[\\x5C]` hex spelling.
