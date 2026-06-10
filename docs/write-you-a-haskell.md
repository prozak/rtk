# Write You a Haskell on RTK

This project reimplements the languages of Stephen Diehl's tutorial
["Write You a Haskell"](https://github.com/sdiehl/write-you-a-haskell)
using RTK as the frontend toolkit. Where the tutorial hand-writes Parsec
parsers and abstract syntax per chapter, here each language is a `.pg`
grammar from which RTK generates the lexer, parser, AST and quasi-quoters;
the semantic passes (evaluation, substitution, type checking, inference)
are written by pattern-matching on *concrete syntax* via the generated
quasi-quoters - something the tutorial itself cannot do.

## Status

| Phase | Tutorial chapters | Language | State |
|-------|-------------------|----------|-------|
| 0 | - | `lc.pg` spike: QQ viability on a ladder grammar | done |
| 1 | 3 (parsing), 4 (untyped LC) | `lc.pg` + `lc-main.hs` interpreter | done |
| 2 | 5-6 (type systems, evaluation) | simply typed LC | planned |
| 3 | 7 (Hindley-Milner) | Poly with `let rec`, inference, REPL | planned |
| 4 | 8-12 (ProtoHaskell) | stretch goal; needs a layout decision | open |

Chapter 3's content (writing a parser combinator library) is replaced by
RTK itself; its calculator example is subsumed by `lc.pg`. Chapters 13+
of the tutorial were never written.

## The lc language (chapters 3-4)

`test-grammars/lc.pg` defines an untyped lambda calculus with the surface
syntax of the tutorial's chapter-7 Poly language: `\x -> e` lambdas,
`let .. in`, `if/then/else`, an `==` / `+ -` / `*` operator ladder,
application by juxtaposition, integer and boolean literals, and `--`
comments. The grammar is conflict-free under happy.

`test-grammars/lc-main.hs` implements, entirely with QQ patterns over the
generated AST: a call-by-value evaluator with closures and environments
(the design of the tutorial's `Eval.hs`), free variables,
capture-avoiding substitution, a precedence-aware pretty-printer, and a
REPL.

```
make test-lc    # build the generated pipeline and run the test suite
make repl-lc    # interactive REPL: lc> (\s -> \z -> s (s z)) (\n -> n + 1) 0
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
