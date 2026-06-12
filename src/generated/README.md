# Self-Hosted Front End

This directory contains `Frontend.hs`, the hand-written half of RTK's
self-hosted front end — the **default** front end since the authority
inversion (grammar.pg is the specification of the grammar language; see
`BOOTSTRAP.md`).

## How it works

```
grammar source
   │
   ├── default:            GrammarLexer.x ──▶ GrammarParser.y ─┐
   │                                                           ├─▶ generated AST (GP.Grammar)
   └── --use-handwritten:  Lexer.x ──catBigstrs──▶ Parser.y ───┘        │
                                                                Frontend.cleanGrammarTokens
                                                                        │
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

Since task 8c there is **no separate pipeline AST**: the whole front half of
the pipeline (`StringLiterals`, the clause normalization in `Normalize`, the
lexical-clause translation in `GenX`) computes directly over the generated
AST. The hand-written reference `Parser.y` builds the very same
`GP.Grammar`/`GP.Rule`/`GP.Clause` values — same constructors, same binary
`Alt`/`Seq` spines, same first-symbol positions — so the two front ends are
interchangeable by type, not by conversion. `Frontend.hs` hosts the parse
entry points, the shared token-text cleanup and the accessors the pipeline
uses over the generated AST.

`GrammarQQ.hs` (the generated quasi-quoter) is compiled in from the same
snapshot (task 8b of `docs/qq-grammar-rewrites-plan.md`): generated
quasi-quoters need no regex packages anymore, only `template-haskell`, so
rtk's own code and tests can quote grammar fragments against the generated
AST — `cabal test unit` smoke-tests `[clause| … |]` quotes and `Anti_*`
splices in-tree. The quotes produce the same types the pipeline computes
over. grammar.pg names every constructor-producing alternative
(`RuleSimple`, `Star`, `Labeled`, …), so the pipeline's pattern matches are
stable prose names rather than positional `Ctr__<Rule>__<index>` ones; the
golden snapshot pins them.

## What the front ends share

1. **Token-text cleanup, one implementation.** Both lexers keep the raw
   token text (quotes around `'str'`, brackets around `[regex]`, triple
   quotes around `"""bigstr"""`, escape pairs intact), and both front ends
   run `Frontend.cleanGrammarTokens` right after parsing: it strips the
   delimiters and applies `TokenProcessing.unBackQuote` to string and regex
   leaves. The escape logic is not duplicated anywhere.
2. **Positions on every node.** Every generated constructor (except the
   quasi-quotation-only `Anti_*` ones) stores the position of its
   alternative's first symbol in a leading `RtkPos` field; the reference
   parser records the same positions. Normalization diagnostics point at the
   offending clause, not just the rule header. `RtkPos` is
   equality-transparent, so the AST-equality suite projects positions
   explicitly to assert agreement.
3. **Error positions.** The generated lexer and parser (and the hand-written
   lexer) encode failures as `LINE:COL:message`; `parseWithGenerated` splits
   them back into positioned `Diagnostic`s via
   `Diagnostics.diagnosticFromPositioned`, so both front ends render the
   same GNU-style `FILE:LINE:COL: error:` prefix.

(The direction of authority: where the two front ends used to disagree on
the language itself — empty alternatives, redundant-paren grouping — the
hand-written parser was changed to follow grammar.pg. See BOOTSTRAP.md.)

## Equivalence guarantees

- `cabal test golden` runs every grammar in `test-grammars/` through **both**
  front ends; both must reproduce the snapshots in `test/golden/`
  byte-for-byte. The pin list for tolerated divergences
  (`TestSupport.frontEndDivergentGrammars`) is empty.
- `cabal test unit` parses every grammar with both front ends and asserts
  the `GP.Grammar`s are equal AND that the projected positions of every node
  agree (equality alone cannot see positions), plus front-end error parity
  on broken inputs.
- The fixed point: `rtk test-grammars/grammar.pg out/` (a default invocation,
  no flag) regenerates `test/golden/grammar/` exactly — RTK parses its own
  grammar with the parser it generated from that grammar.

## The bootstrap coupling (now structural)

The pipeline's source code is typed against its own generated output: a
grammar.pg change that alters the generated AST's shape or constructor names
breaks the in-tree build until the snapshot is re-accepted AND the
pipeline's pattern matches are updated. See "Changing the grammar language"
in `BOOTSTRAP.md` for the two-phase workflow.

## Known divergences (documented, accepted)

See `BOOTSTRAP.md` for the full list: parse-error *wording* differs (same
structured position), no nested `/* /* */ */` comments (GitHub issue #25),
and concatenation of adjacent `"""…"""` blocks exists only on the reference
path.
