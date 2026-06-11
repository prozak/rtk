# Self-Hosted Front End

This directory contains `ASTAdapter.hs`, the hand-written half of RTK's
self-hosted front end (`rtk --use-generated`).

## How it works

```
grammar source
   │
   ├── default:          Lexer.x ──processTokens──▶ Parser.y ──▶ InitialGrammar
   │
   └── --use-generated:  GrammarLexer.x ──▶ GrammarParser.y ──▶ generated AST
                                                      │
                                            ASTAdapter.convertGrammar
                                                      │
                                                      ▼
                                               InitialGrammar
   ▼
shared pipeline: string-norm → clause-norm → fill-names → genX/genY/genQ
```

`GrammarLexer.x` and `GrammarParser.y` are the lexer and parser that RTK
generates from `test-grammars/grammar.pg`. They are **not** stored here: the
library compiles them directly from the golden snapshot in
`test/golden/grammar/` (see `Hs-Source-Dirs` in `rtk.cabal`). That snapshot is
the checked-in bootstrap stage — it was produced by the *previous* rtk binary,
exactly like the stage files of any self-hosting compiler — and
`make accept-golden` refreshes it, so the build input of the generated front
end stays in sync with the generators by construction.

`GrammarQQ.hs` (the generated quasi-quoter) is deliberately **not** compiled
into rtk: it would drag `regex-posix` and Template Haskell splices into the
build. An earlier sketch of the adapter used quasi-quotation to pattern match
on the generated AST; the real adapter is plain total pattern matching on the
generated constructors (`Ctr__…`), which is deterministic and dependency-free.
The golden snapshot pins those constructor names.

## What the adapter must replicate

Two things the hand-written front end does between lexing and parsing:

1. **Delimiter stripping.** The hand-written lexer strips literal delimiters
   from token text (quotes around `'str'`, brackets around `[regex]`, triple
   quotes around `"""bigstr"""`); the generated lexer keeps the full match,
   so the adapter strips them.
2. **Escape processing.** `TokenProcessing.processTokens` applies
   `unBackQuote` to string and regex literals. The adapter reuses the
   exported `unBackQuote` on the same leaves — the escape logic is not
   duplicated.

Source positions are **not** captured yet (task 7b): `getIRulePos` is
`Nothing` everywhere. Generated artifacts never embed source positions —
positions only affect error messages — so this cannot change the generated
output for a valid grammar.

## Equivalence guarantees

- `cabal test golden` runs every grammar in `test-grammars/` through **both**
  front ends; both must reproduce the snapshots in `test/golden/`
  byte-for-byte (three grammars that use hand-parser-only syntax are pinned
  in `TestSupport.frontEndDivergentGrammars` with a still-diverges guard).
- `cabal test unit` parses every grammar with both front ends and asserts the
  `InitialGrammar`s are equal after stripping positions (same pins).
- The fixed point: `rtk --use-generated test-grammars/grammar.pg out/`
  regenerates `test/golden/grammar/` exactly — RTK parses its own grammar
  with the parser it generated from that grammar.

## Known divergences (documented, accepted)

See `BOOTSTRAP.md` for the full list: `Either String` errors with positions
in the message text (structured positions come with task 7b), no nested
`/* /* */ */` comments (GitHub issue #25), no concatenation of adjacent
`"""…"""` blocks, no empty alternatives (`Gd = | ExpI ;` is
hand-parser-only syntax), and no redundant-parenthesis grouping
(grammar.pg lifts paren groups, the hand-written parser keeps them).
