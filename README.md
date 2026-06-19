# RTK - Rewrite ToolKit

RTK generates parser and rewrite facilities from grammar specifications. It produces Alex lexer and Happy parser files, with support for quasi-quotation to embed parsed syntax directly in Haskell code.

## Features

- **Grammar Specifications**: Define languages using `.pg` grammar files
- **Lexer Generation**: Generates Alex (`.x`) lexer specifications
- **Parser Generation**: Generates Happy (`.y`) parser specifications
- **Quasi-Quotation**: Embed parsed syntax in Haskell via Template Haskell
- **Self-Hosting**: RTK parses grammar files with the parser it generated
  from its own grammar description (`test-grammars/grammar.pg`) — by default.
  The hand-written front end is kept as a reference oracle behind
  `--use-handwritten`; see [BOOTSTRAP.md](BOOTSTRAP.md)

## Installation

```bash
cabal update
cabal install rtk
```

## Usage

Generate lexer and parser from a grammar file:

```bash
rtk <grammar-file>.pg <output-directory>
```

This creates:
- `<Grammar>Lexer.x` - Alex lexer specification
- `<Grammar>Parser.y` - Happy parser specification
- `<Grammar>QQ.hs` - Quasi-quoter module

Then compile with Alex and Happy:

```bash
alex <Grammar>Lexer.x -o <Grammar>Lexer.hs
happy <Grammar>Parser.y --ghc -o <Grammar>Parser.hs
```

#### Optional: a pretty-printer (`--generate-pp`)

`--generate-pp` writes a fifth, opt-in artifact `<Grammar>PP.hs`: a
`base`-only module of `pp<Type>` functions that turn a parsed AST back into
source text. It guarantees only the *semantic* round-trip
`parse (print ast) == ast`, never byte-faithful reproduction: comments and the
original whitespace are lost because the AST is lossy. The flag is off by
default, so output is unchanged unless you ask for it.

Two layouts are available via `--pp-layout`:

- `flat` (default) — one space between tokens, no indentation; correct, not
  pretty.
- `block` — indents and line-breaks bracket-structured languages (C-like
  braces, PL/0-style `begin`/`end`) so output reads like hand-written source.
  Indentation is derived structurally from statement/declaration lists, so it
  adds no parentheses and degrades to flat for grammars without such lists.

Layout is whitespace, so `block` never changes the parse — it is heuristic
readability, and the round-trip guarantee holds in either mode.

### Using the generated code

The generated modules are compiled as part of *your* project, so your project
must depend on the packages they use:

- `array` — runtime support for the Alex lexer and the Happy parser tables
- `syb` — the generated parser and quasi-quoter use `Data.Generics`
- `containers` — the quasi-quoter keeps its shortcut table in a `Data.Map`
- `template-haskell` — the quasi-quoter builds `Language.Haskell.TH` splices

If you only use the lexer and parser (no quasi-quotation), `array` and `syb`
are enough. A typical `build-depends` line for code that uses all three
generated modules:

```
build-depends: base, array, syb, containers, template-haskell
```

The quasi-quoter is also the rewrite facility: quasi-quoted patterns as
match arms plus SYB's `everywhere`/`everything` rewrite and query parsed
ASTs with no further API — see "Rewriting parsed Java" in
[docs/java-quasi-quotation-tests.md](docs/java-quasi-quotation-tests.md)
for the worked recipe (rtk's own pipeline and the
write-you-a-haskell tutorial use the same shape).

## Grammar Format

Grammar files use a simple specification format. Each file starts with a
`grammar 'Name';` header. A rule is a syntax rule if its name begins with an
uppercase letter and a lexical rule if it begins with a lowercase letter.
A rule may carry an optional `Type:` data-type annotation before its name
(as in `Int: num = …` below — the rule name is `num`; `Int` is the
annotation). `'…'` matches a string literal, `[…]` a character class, and
`* + ?` denote repetition. Constructors for the AST are generated
automatically — there are no inline semantic actions.

```
grammar 'Calc';

# Syntax rules: name starts with an uppercase letter
Expr = Term ('+' Term)* ;
Term = num ;

# Lexical rules: name starts with a lowercase letter
# ('Int:' and 'Ignore:' are data-type annotations, not rule names)
Int:    num = [0-9]+ ;
Ignore: ws  = [ \t\n]+ ;
```

### Named constructors

By default the constructor generated for an alternative is positional
(`Ctr__<Rule>__<index>`), so inserting or reordering alternatives silently
renames constructors. An alternative may opt in to a stable name with a
leading label:

```
Expr = Add: Expr '+' Term
     | Sub: Expr '-' Term
     | Term ;
```

generates `data Expr = Add RtkPos Expr Term | Sub RtkPos Expr Term |
Ctr__Expr__0 RtkPos Term | ...` — code and quasi-quote patterns written
against `Add`/`Sub` survive grammar edits. The label binds tighter than `|`
and names exactly one alternative; it also works inside parenthesized
groups (`(Pair: key '=' value)*` names the extracted group's constructor).
Unlabeled alternatives keep their generated names. Explicit names must
start with an uppercase letter, must be unique across the whole grammar
(all constructors share one generated module), must avoid the reserved
`Ctr__`/`Anti_` prefixes, and cannot name a lifted (`,Rule`) alternative —
it passes a value through and produces no constructor. rtk rejects each of
these with a positioned diagnostic.

See `test-grammars/grammar.pg` for the grammar language described in itself —
that file is the authoritative definition of the grammar language: rtk parses
your grammar with the parser it generated from it (self-hosting).

## Example Grammars

The `test-grammars/` directory contains example grammars:
- `java.pg` - Java language grammar
- `grammar.pg` - Grammar for the grammar language itself (bootstrap)
- `haskell.pg` - Haskell subset grammar

The `tutorials/` directory contains self-contained projects built with RTK,
including a C compiler and a port of Peter Norvig's lis.py Lisp interpreter
(quasi-quotation for special-form dispatch and macro expansion); see
[tutorials/README.md](tutorials/README.md).

## Building from Source

Requirements:
- GHC >= 9.4
- Cabal >= 3.8
- Alex
- Happy

```bash
cabal build
cabal test
```

## License

MIT License - see [LICENSE](LICENSE) for details.

Generated code (lexers, parsers, quasi-quoters) produced by RTK is exempt from this license and may be used without restriction.
