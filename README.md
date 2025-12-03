# RTK - Rewrite ToolKit

RTK generates parser and rewrite facilities from grammar specifications. It produces Alex lexer and Happy parser files, with support for quasi-quotation to embed parsed syntax directly in Haskell code.

## Features

- **Grammar Specifications**: Define languages using `.pg` grammar files
- **Lexer Generation**: Generates Alex (`.x`) lexer specifications
- **Parser Generation**: Generates Happy (`.y`) parser specifications
- **Quasi-Quotation**: Embed parsed syntax in Haskell via Template Haskell
- **Self-Hosting**: RTK can parse and generate parsers for its own grammar format

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
happy <Grammar>Parser.y -o <Grammar>Parser.hs
```

## Grammar Format

Grammar files use a simple specification format:

```
grammar MyLang;

// Token definitions
INT = [0-9]+;
PLUS = "+";

// Grammar rules
Expr : Expr PLUS Term { Add $1 $3 }
     | Term           { $1 }
     ;

Term : INT { Lit (read $1) }
     ;
```

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
