# Self-Hosted Front End

This directory contains `ASTAdapter.hs`, the hand-written half of RTK's
self-hosted front end — the **default** front end since the authority
inversion (grammar.pg is the specification of the grammar language; see
`BOOTSTRAP.md`).

## How it works

```
grammar source
   │
   ├── default:            GrammarLexer.x ──▶ GrammarParser.y ──▶ generated AST
   │                                                  │
   │                                        ASTAdapter.convertGrammar
   │                                                  │
   │                                                  ▼
   │                                           InitialGrammar
   │
   └── --use-handwritten:  Lexer.x ──processTokens──▶ Parser.y ──▶ InitialGrammar
   ▼
shared pipeline: string-norm → clause-norm → fill-names → genX/genY/genQ
```

`GrammarLexer.x` and `GrammarParser.y` are the lexer and parser that RTK
generates from `test-grammars/grammar.pg`. They are **not** stored here: the
library compiles them directly from the golden snapshot in
`test/golden/grammar/` (see `Hs-Source-Dirs` in `rtk.cabal`). That snapshot is
the checked-in bootstrap stage — it was produced by the *previous* rtk binary,
exactly like the stage files of any self-hosting compiler — and
`make accept-golden` advances it, so the build input of the default front end
stays in sync with the generators by construction.

`GrammarQQ.hs` (the generated quasi-quoter) is deliberately **not** compiled
into rtk: it would drag `regex-posix` and Template Haskell splices into the
build. An earlier sketch of the adapter used quasi-quotation to pattern match
on the generated AST; the real adapter is plain total pattern matching on the
generated constructors (`Ctr__…`), which is deterministic and dependency-free.
The golden snapshot pins those constructor names.

## What the adapter must replicate

Things the hand-written reference front end does between lexing and parsing:

1. **Delimiter stripping.** The hand-written lexer strips literal delimiters
   from token text (quotes around `'str'`, brackets around `[regex]`, triple
   quotes around `"""bigstr"""`); the generated lexer keeps the full match,
   so the adapter strips them.
2. **Escape processing.** `TokenProcessing.processTokens` applies
   `unBackQuote` to string and regex literals. The adapter reuses the
   exported `unBackQuote` on the same leaves — the escape logic is not
   duplicated.
3. **Rule positions.** Every generated constructor (except the
   quasi-quotation-only `Anti_*` ones) stores the position of its first
   token in a leading `RtkPos` field; the adapter maps the rule
   constructors' positions into `getIRulePos`, so diagnostics under either
   front end point at the same source locations.
4. **Error positions.** The generated lexer and parser encode failures as
   `LINE:COL:message` (the same encoding the hand-written lexer uses);
   `parseWithGenerated` splits them back into positioned `Diagnostic`s via
   `Diagnostics.diagnosticFromPositioned`, so both front ends render the
   same GNU-style `FILE:LINE:COL: error:` prefix.

(The direction of authority is the reverse: where the two front ends used to
disagree on the language itself — empty alternatives, redundant-paren
grouping — the hand-written parser was changed to follow grammar.pg, not the
adapter to follow the hand-written parser. See BOOTSTRAP.md.)

## Equivalence guarantees

- `cabal test golden` runs every grammar in `test-grammars/` through **both**
  front ends; both must reproduce the snapshots in `test/golden/`
  byte-for-byte. The pin list for tolerated divergences
  (`TestSupport.frontEndDivergentGrammars`) is empty.
- `cabal test unit` parses every grammar with both front ends and asserts the
  `InitialGrammar`s are equal, source positions included, plus front-end
  error parity on broken inputs.
- The fixed point: `rtk test-grammars/grammar.pg out/` (a default invocation,
  no flag) regenerates `test/golden/grammar/` exactly — RTK parses its own
  grammar with the parser it generated from that grammar.

## Known divergences (documented, accepted)

See `BOOTSTRAP.md` for the full list: parse-error *wording* differs (same
structured position), no nested `/* /* */ */` comments (GitHub issue #25),
and no concatenation of adjacent `"""…"""` blocks.
