# RTK Debug Options

This document describes all debugging options available in RTK (Rewrite Toolkit) to simplify grammar development and debugging.

## Usage

```bash
rtk <grammar-file> <output-dir> [OPTIONS]
```

## Front-End Selection

By default RTK parses grammars with its **self-hosted front end**: the
lexer/parser RTK generated from `test-grammars/grammar.pg` (the authoritative
definition of the grammar language), compiled from the checked-in snapshot in
`test/golden/grammar/`. The pipeline computes directly over that generated
AST (the reference front end builds the very same AST), and everything after
parsing is the same shared pipeline, so the generated files are
byte-identical whichever front end parsed the grammar. See `BOOTSTRAP.md`.

### `--use-generated`
Explicitly select the self-hosted front end. This is the default, so the flag
is a no-op kept for compatibility and for being explicit in scripts.

### `--use-handwritten`
Parse the grammar with the hand-written reference `Lexer.x`/`Parser.y`
instead. The reference front end exists as the oracle for the self-hosting
equivalence harness; output artifacts are identical to the default's.

**Example:**
```bash
rtk test-grammars/grammar.pg test-out                    # generated (default)
rtk --use-handwritten test-grammars/grammar.pg test-out  # reference
```

**Notes:** lexical and parse errors carry a structured `FILE:LINE:COL:`
prefix under both front ends (parse-error *wording* differs slightly: the
generated parser renders tokens generically). In default mode there is no
token post-processing stage, so `--debug-tokens` shows the raw generated
token stream and `--debug-stage lex` stops after the combined front end.

## Pipeline Stage Inspection Options

### `--debug-tokens` / `-t`
Print all tokens after lexical analysis.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-tokens
```

**Output:** Pretty-printed list of tokens with counts.

### `--debug-parse` / `-p`
Print the parsed grammar (the generated AST) after parsing.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-parse
```

**Use case:** Verify parser correctly interprets grammar rules.

### `--debug-string-norm`
Print grammar before and after string literal normalization.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-string-norm
```

**Use case:** Debug string literal handling issues.

### `--debug-clause-norm`
Print NormalGrammar after clause normalization.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-clause-norm
```

**Use case:** Understand how complex clauses are simplified.

### `--debug-constructors` / `-c`
Print grammar after constructor name generation.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-constructors
```

**Use case:** Verify constructor naming, debug name collisions.

## Output Stage Inspection Options

### `--debug-parser-spec`
Print generated Happy parser specification (`.y` file content).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-parser-spec > java-parser.y
```

**Use case:** Debug parser conflicts, understand generated parser.

### `--debug-lexer-spec`
Print generated Alex lexer specification (`.x` file content).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-lexer-spec > java-lexer.x
```

**Use case:** Debug lexer rules, verify token definitions.

### `--debug-qq-spec`
Print generated quasiquoter code (`.hs` file content).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-qq-spec
```

**Use case:** Debug quasiquoter generation issues.

### `--debug-pp-spec`
Print generated pretty-printer code (`<Name>PP.hs` content) without writing
the file. Implies generation of the printer for the dump only.

**Example:**
```bash
rtk test-grammars/p.pg test-out --debug-pp-spec
```

**Use case:** Inspect the generated `pp<Type>` functions for a grammar.

## Optional Generators

### `--generate-pp`
Also write `<Name>PP.hs`, a pretty-printer over the AST RTK generates for the
grammar (the "emit" third of "Rewrite ToolKit"). Off by default: without this
flag the output is exactly the usual `<Name>Lexer.x` / `<Name>Parser.y` /
`<Name>QQ.hs`, byte-for-byte unchanged.

The generated module depends only on `base` (it imports its grammar's
`<Name>Parser` AST types and `Data.List`). It defines one `pp<Type>` per
rule-group type; for the start type that is the printer you call on a parsed
AST.

**v1 is correctness-first, not pretty.** It emits exactly one space between
tokens, with no indentation or alignment, and adds no parentheses of its own.
The only guarantee is the semantic round-trip `parse (print ast) == ast` —
not byte-faithful reproduction: comments and the original whitespace are lost
because the AST is lossy. Repetition over an un-parenthesized alternation does
not round-trip in v1 (a structural printer re-associates it on reparse);
auto-parenthesization is a later task. The `make test-pp` target is the
round-trip oracle.

**Example:**
```bash
rtk test-grammars/p.pg test-out --generate-pp   # also writes test-out/PPP.hs
```

### `--pp-layout=flat|block`
Choose the pretty-printer's layout (only meaningful with `--generate-pp` or
`--debug-pp-spec`).

- `flat` (default) — one space between tokens, no indentation; the
  correct-not-pretty layout described above. Byte-for-byte unchanged.
- `block` — indents and line-breaks bracket-structured languages so output
  reads like hand-written source. Indentation comes purely from *block lists*
  (statement/declaration lists), so the `{` `}` / `begin` `end` delimiters
  need no special handling and a grammar with no block lists degrades to flat.

Both layouts keep the same guarantee (`parse (print ast) == ast`) and the same
`base`-only dependency (block mode adds a small in-module layout engine).
Layout is whitespace, which every grammar in the corpus ignores, so block
layout never changes the parse — it is heuristic *readability*, not
correctness. Quality is language-dependent: bracket/terminator languages
(C-like, PL/0's `begin`/`end`) read well; preamble-heavy grammars may run
together — see the task 9b notes in `CHANGELOG.md`.

**Example:**
```bash
rtk tutorials/c-compiler/c.pg out --generate-pp --pp-layout=block
```

## Analysis and Statistics Options

### `--stats` / `-s`
Print comprehensive compilation statistics.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --stats
```

**Output:**
```
Grammar Statistics:
  - Rules: 127
  - Syntax rule groups: 15
  - Lexical rules: 42
  - Proxy rules: 8
  - Auto-generated constructors: 234
```

### `--analyze-conflicts`
Analyze grammar for potential parser conflicts.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --analyze-conflicts
```

**Use case:** Proactively identify grammar issues before compilation.

### `--show-rule-graph`
Show dependency graph between rules.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --show-rule-graph
```

**Output:** Text representation showing which rules reference which.

### `--list-rules`
List all rule names by category.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --list-rules
```

**Output:** Organized list of syntax rules, lexical rules, and macros.

## Selective Debug Options

### `--debug-rule=RULENAME`
Trace a single rule through the transformation pipeline. After each stage
(tokens, parse, string normalization, clause normalization, constructor
fill) only that rule's representation is printed instead of a full-grammar
dump. Where `--expand-rule` shows a rule's *final* expanded form, this flag
shows its *evolution* — e.g. string normalization rewriting literals into
`!tok_*` references, or clause normalization adding generated rules to the
rule's group.

If the rule is missing at some stage, the trace says so and lists up to
five case-insensitive near matches present at that stage (normalization
renames things — `Rule_N`, `ListElem_*`, `tok_*`). A rule that matches at
no stage at all fails the run with exit code 1, so typos are caught in
scripts. Composes with `--debug-stage` (stop early) and with
`--use-generated` (the token stage is internal to the generated front end,
so it is reduced to a note and the trace starts after parsing).

**Example:**
```bash
rtk test-grammars/grammar.pg test-out --debug-rule=Clause
```

**Output (abridged):**
```
======================================================================
  RULE TRACE: 'Clause' - Tokens
======================================================================
Mentioned 12 time(s) in the token stream:
  line 33, column 36: Id "Clause"
  line 46, column 1: Id "Clause"
  ...

======================================================================
  RULE TRACE: 'Clause' - After Parse
======================================================================
-- Rule 'Clause' (line 46, column 1)
RuleWithOptions
  (RtkPos (AlexPn ...))
  [ Shortcuts (RtkPos ...) [ Ident (RtkPos ...) "cl" ] ]
  (RuleSimple
     (RtkPos ...)
     (Ident (RtkPos ...) "Clause")
     (Alt
        (RtkPos ...)
        (Labeled (RtkPos ...) (Ident (RtkPos ...) "Alt")
           (Seq (RtkPos ...)
              (Seq (RtkPos ...) (Ref ... "Clause") (Lit ... "|"))
              (Ref ... "Clause1")))
        (Lifted (RtkPos ...) (Ref ... "Clause1"))))
-- Rule 'Clause1' (line 53, column 1)  [matches via its 'Clause' data type]
...

======================================================================
  RULE TRACE: 'Clause' - After String Normalization
======================================================================
-- Rule 'Clause' (line 46, column 1)
...
     (Alt
        (RtkPos ...)
        (Labeled ... "Alt"
           (Seq ...
              (Seq ... (Ref ... "Clause")
                       (Ignored ... (Ref ... "tok__pipe__11")))  -- was Lit "|"
              (Ref ... "Clause1")))
        ...)
...

======================================================================
  RULE TRACE: 'Clause' - After Clause Normalization
======================================================================
  Clause (6 rules)
    - Clause5: 6 alternatives
    - Clause4: 6 alternatives
    - Clause3: 3 alternatives
    - Clause2: 2 alternatives
    - Clause1: 2 alternatives
    - Clause: 2 alternatives
SyntaxRuleGroup
  { getSDataTypeName = "Clause"
  , getSRules =
      [ SyntaxRule
          { getSRuleName = "Clause5"
          , getSClause =
              STAltOfSeq
                { getAltOfSeq =
                    [ STSeq "Anti_Clause" [ SSId "qq_Clause" ]
                    , ... ] } }
      , ... ] }

======================================================================
  RULE TRACE: 'Clause' - After Constructor Fill
======================================================================
  ...same group; named alternatives keep their labels and the unnamed
  (here: lifted) ones get generated names filled in:
                    [ STSeq "Anti_Clause" [ SSId "qq_Clause" ]
                    , STSeq
                        "Ctr__Clause__0"
                        [ SSIgnore "tok__lparen__7"
                        , SSLifted "Clause"
                        , SSIgnore "tok__rparen__8"
                        ]
                    , STSeq "Ref" [ SSId "Name" ]
                    , STSeq "Lit" [ SSId "StrLit" ]
                    , ... ]
```

**Use case:** Deep-dive debugging of problematic rules.

### `--debug-stage=STAGE`
Stop after a specific stage and dump state.

**Stages:** `lex`, `parse`, `string-norm`, `clause-norm`, `fill-names`, `gen`

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-stage=clause-norm
```

**Use case:** Incremental debugging of transformation pipeline.

## Validation Options

### `--validate-grammar`
Run validation checks without generating output files.

**Checks:**
- Undefined rule references
- Duplicate constructor names
- Unused rules
- Ambiguous patterns

**Example:**
```bash
rtk test-grammars/java.rtk test-out --validate-grammar
```

### `--unused-rules`
Find rules that are defined but never referenced.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --unused-rules
```

**Use case:** Grammar cleanup.

### `--check-left-recursion`
Detect left-recursive rules that might cause issues.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --check-left-recursion
```

### `--suggest-shortcuts`
Analyze grammar and suggest common patterns for `@shortcuts`.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --suggest-shortcuts
```

**Use case:** Optimize repetitive patterns.

### `--expand-rule=RULENAME`
Show fully expanded form of a rule (inline all references).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --expand-rule=classDeclaration
```

**Use case:** Understand complex nested rules.

## Output Format Options

Debug output is always pretty-printed. (An earlier `--debug-format` option
offered a `compact` mode that merely stripped newlines into an unreadable
single line, and at one point advertised never-implemented `json`/`tree`
modes that silently fell back to pretty; the option was removed rather than
kept as a one-value choice.)

### `--debug-color`
Enable colored output for better readability (enabled by default).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --stats --debug-color
```

## Performance and Profiling Options

### `--profile-stages`
Show timing for each compilation stage.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --profile-stages
```

**Output:**
```
Stage Timings:
  Lexical Analysis:    12ms
  Parsing:             45ms
  String Normalization: 3ms
  Clause Normalization: 127ms
  Constructor Fill:     8ms
  Code Generation:     89ms
  Total:              284ms
```

Each stage result is forced to normal form while being timed, so the
reported durations reflect the work of that stage rather than work
deferred by lazy evaluation.

## Common Workflows

### Quick Grammar Validation
```bash
rtk grammar.rtk out --validate-grammar --stats
```

### Debug Parser Conflicts
```bash
rtk grammar.rtk out --analyze-conflicts --debug-parser-spec
```

### Performance Optimization
```bash
rtk grammar.rtk out --profile-stages
```

### Complete Grammar Analysis
```bash
rtk grammar.rtk out --stats --list-rules --show-rule-graph --unused-rules --check-left-recursion
```

### Deep Debugging a Specific Rule
```bash
rtk grammar.rtk out --debug-rule=myRule --expand-rule=myRule
```

## Tips

1. **Start with `--stats`** to get a quick overview of your grammar
2. **Use `--validate-grammar`** early to catch common mistakes
3. **Combine options** for comprehensive analysis: `--stats --analyze-conflicts --unused-rules`
4. **Use `--debug-stage`** for incremental debugging when the full pipeline is slow
5. **Redirect output** to files for large grammars: `rtk grammar.rtk out --debug-parse > parse-output.txt`

## Implementation Status

All options documented above are fully implemented. Earlier placeholder
options that were advertised but never implemented (`--compare-stages`,
`--memory-stats`, `--debug-output-dir`, `--debug-log`, `--interactive`,
and the whole `--debug-format` option with its `json`/`tree`/`compact`
formats) have been removed from the CLI; they can be reintroduced together
with real implementations, as `--debug-rule` has been.

## See Also

- Main README for general RTK usage
- Grammar syntax documentation
- Examples in `test-grammars/` directory
