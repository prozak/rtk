# RTK Bootstrap Self-Hosting

## Status: self-hosting achieved — grammar.pg is the specification

RTK parses grammar files **by default** with the lexer and parser it generated
from its own grammar description, `test-grammars/grammar.pg`. Like a C
compiler compiled by itself, RTK is self-hosting:

- **`test-grammars/grammar.pg` is the authoritative definition of the grammar
  language.** Changes to the grammar language land in grammar.pg (and the
  regenerated goldens) FIRST.
- **The generated front end is the default.** `GrammarLexer.x` /
  `GrammarParser.y` / `GrammarQQ.hs` — RTK's own output for grammar.pg — are
  compiled into rtk straight from the golden snapshot in `test/golden/grammar/`.
  Since task 8c the pipeline computes DIRECTLY over the generated AST
  (`GP.Grammar`/`GP.Rule`/`GP.Clause`): there is no separate pipeline
  representation and no conversion layer. `src/generated/Frontend.hs` hosts
  the parse entry points, the shared token-text cleanup
  (`cleanGrammarTokens`) and the accessors the pipeline uses.
  `--use-generated` is still accepted as an explicit choice. (The compiled-in
  quasi-quoter quotes fragments as the same generated AST types the pipeline
  computes over; the unit suite smoke-tests it.)
- **The hand-written `Lexer.x` / `Parser.y` are the reference oracle.** They
  are selected with `rtk --use-handwritten`, and `Parser.y`'s semantic
  actions build the very same generated-AST values (same constructors, same
  binary `Alt`/`Seq` spines, same first-symbol positions). They exist to
  keep the equivalence harness honest: they follow grammar.pg, not the other
  way round, and change only to keep the harness green.

The snapshot in `test/golden/grammar/` is the checked-in bootstrap stage: it
was produced by the *previous* rtk binary, exactly like the stage files of any
self-hosting compiler, and `make accept-golden` advances it.

## The fixed point

A default invocation regenerates RTK's own front end byte-for-byte:

```bash
cabal run rtk -- test-grammars/grammar.pg /tmp/out
diff /tmp/out/GrammarLexer.x  test/golden/grammar/GrammarLexer.x   # identical
diff /tmp/out/GrammarParser.y test/golden/grammar/GrammarParser.y  # identical
diff /tmp/out/GrammarQQ.hs    test/golden/grammar/GrammarQQ.hs     # identical
```

RTK parses its own grammar with the parser it generated from that grammar and
reproduces that parser exactly.

## The equivalence harness (the standing oracle)

Both front ends are exercised on every test run; the harness is what makes the
hand-written front end a usable reference:

- `cabal test golden` runs **every** grammar in `test-grammars/` through both
  front ends; both must reproduce the snapshots in `test/golden/`
  byte-for-byte.
- `cabal test unit` parses every grammar with both front ends and asserts
  the `GP.Grammar`s are equal AND that the projected (line, column) of every
  AST node agrees — `RtkPos` is equality-transparent, so equality alone
  cannot see positions and the suite projects them explicitly. It also
  asserts front-end error parity: lexical errors render identically under
  both front ends, and parse errors carry the same structured position.
- `TestSupport.frontEndDivergentGrammars` is the pin list for grammars whose
  front ends are temporarily allowed to diverge. **It is empty.** Should a
  divergence ever have to be tolerated again, a pinned grammar is checked with
  the reference front end only, and both suites fail as soon as it stops
  diverging so the pin gets dropped.

(The earlier `compare-bootstrap.sh` textual comparison of the generated
`.x`/`.y` against the hand-written ones is retired: behavioral equivalence is
enforced by the harness above, and full textual identity was never the goal.)

## Changing the grammar language

The coupling is **structural** since task 8c: the pipeline's own source code
(`Frontend`, `StringLiterals`, `Normalize`, `GenX`, the hand-written
`Parser.y`, parts of the test suites) is typed against the AST compiled from
the snapshot. A grammar.pg change that alters the generated AST's shape or
constructor names therefore breaks the in-tree BUILD — not just a test —
until the snapshot is re-accepted and the pipeline's pattern matches are
updated. The workflow is two-phase:

1. Edit `test-grammars/grammar.pg` — the spec.
2. `make accept-golden` and review the diff of `test/golden/` (this also
   advances the bootstrap stage that the default front end is compiled
   from). If the checked-in stage cannot parse the new grammar.pg (a change
   to syntax the stage itself doesn't know yet), bootstrap via
   `rtk --use-handwritten` first.
3. Rebuild. If the change altered the generated AST's shape or constructor
   names, fix every pipeline module that pattern-matches the changed
   constructors until the build is clean again — the compiler enumerates the
   sites. grammar.pg names every constructor-producing alternative
   (`RuleSimple`, `Star`, `Labeled`, …), so these matches are stable prose
   names: reordering or inserting alternatives does not rename constructors.
4. Update the hand-written `Lexer.x` / `Parser.y` so the reference follows the
   spec (its actions construct the changed AST) and the equivalence harness
   is green again.
5. `cabal test` — both suites must pass — and re-check the fixed point.

## Error reporting parity

Lexer and parser errors carry structured positions under both front ends and
render in GNU one-line style. The same broken grammar produces the SAME stderr
line through either front end for lexical errors:

```
$ rtk broken.pg out                    # default (generated) front end
broken.pg:2:5: error: lexical error. Following chars: % ;
$ rtk --use-handwritten broken.pg out  # reference front end
broken.pg:2:5: error: lexical error. Following chars: % ;
```

Mechanically: generated lexers/parsers (and the hand-written lexer) encode
errors as `LINE:COL:message`; `Diagnostics.diagnosticFromPositioned` splits
the encoding back into a positioned diagnostic. Generated quasi-quoters and
standalone drivers re-render the encoding human-readably (`line L, column C:
…`) for their `fail`/console paths.

## Known divergences (accepted and documented)

The historic divergences 4 and 5 (empty alternatives, redundant-paren
grouping) are **resolved**: the reference parser now defines the same language
as grammar.pg — it rejects empty alternatives and lifts parenthesis groups
exactly like grammar.pg's `Clause5 = '(' ,Clause ')'` does — and haskell.pg's
`Gd = | ExpI ;` was rewritten as the equivalent `Gd = ExpI? ;`. What remains:

1. **Parse-error wording.** Both front ends report the same structured
   position, but the message text differs: the reference parser knows the
   grammar language's token names (`unexpected identifier 'Foo', followed by:
   …`), while generated parsers render tokens generically (`unexpected id
   "Foo"`). Lexical errors are identical character for character.
2. **Nested comments.** The generated lexer cannot lex nested
   `/* /* */ */` comments (the reference lexer can; GitHub issue #25). No
   corpus grammar nests comments — and per the authority inversion, the spec
   (grammar.pg) does not define nested comments today.
3. **Adjacent `"""…"""` blocks.** The reference path concatenates adjacent
   triple-quoted blocks (`catBigstrs`); grammar.pg accepts a single block
   after `imports`. Only `grammar.pg` uses `imports`, with one block, so this
   is theoretical today.

None of these affect generated artifacts for any corpus grammar — the
equivalence harness proves it on every run.

## History: the path that got here

1. ✅ Grammar completeness: grammar.pg parses the whole corpus
2. ✅ Artifact equivalence for every corpus grammar
3. ✅ Dual-mode entry point (`--use-generated`)
4. ✅ Bootstrap fixed point (`rtk --use-generated grammar.pg` regenerated its
   own parser byte-for-byte)
5. ✅ Structured positions in the generated path (position-transparent
   `RtkPos` on every node)
6. ✅ Front-end error parity (`LINE:COL:` encoding split into diagnostics)
7. ✅ **Generated front end made the default**; hand-written files demoted to
   reference; pinned-divergence list emptied
8. ✅ **Pipeline migrated onto the generated AST** (task 8c): the historic
   `InitialGrammar`/`IRule`/`IClause` types and the AST adapter retired; the
   reference `Parser.y` ported to construct generated-AST values

## Files

- `test-grammars/grammar.pg` — the grammar language specification
- `test/golden/grammar/` — checked-in bootstrap stage (compiled into rtk)
- `src/generated/Frontend.hs` — front-end entry points, shared token-text
  cleanup, generated-AST helpers
- `Lexer.x`, `Parser.y` — hand-written reference front end
  (`--use-handwritten`); the parser's actions build generated-AST values
- `test/TestSupport.hs` — equivalence-harness support (pin list, both
  pipelines)

## Further Reading

- [Bootstrapping Compilers](https://en.wikipedia.org/wiki/Bootstrapping_(compilers))
- Self-hosting examples: GCC, Rust compiler, TypeScript
