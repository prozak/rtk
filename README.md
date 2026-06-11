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

### Using the generated code

The generated modules are compiled as part of *your* project, so your project
must depend on the packages they use:

- `array` — runtime support for the Alex lexer and the Happy parser tables
- `syb` — the generated parser and quasi-quoter use `Data.Generics`
- `containers` — the quasi-quoter keeps its shortcut table in a `Data.Map`
- `template-haskell` — the quasi-quoter builds `Language.Haskell.TH` splices
- `regex-posix` and `regex-base` — the quasi-quoter matches `$var` antiquotation
  patterns with `Text.Regex.Posix`

If you only use the lexer and parser (no quasi-quotation), `array` and `syb`
are enough. A typical `build-depends` line for code that uses all three
generated modules:

```
build-depends: base, array, syb, containers, template-haskell, regex-posix, regex-base
```

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

See `test-grammars/grammar.pg` for the grammar language described in itself —
that file is the authoritative definition of the grammar language: rtk parses
your grammar with the parser it generated from it (self-hosting).

## Example Grammars

The `test-grammars/` directory contains example grammars:
- `java.pg` - Java language grammar
- `grammar.pg` - Grammar for the grammar language itself (bootstrap)
- `haskell.pg` - Haskell subset grammar

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
