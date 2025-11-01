# RTK Debug Options

This document describes all debugging options available in RTK (Rewrite Toolkit) to simplify grammar development and debugging.

## Usage

```bash
rtk <grammar-file> <output-dir> [OPTIONS]
```

## Pipeline Stage Inspection Options

### `--debug-tokens` / `-t`
Print all tokens after lexical analysis.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-tokens
```

**Output:** Pretty-printed list of tokens with counts.

### `--debug-parse` / `-p`
Print the InitialGrammar structure after parsing.

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
Debug a specific rule through all transformation stages.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-rule=compilationUnit
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

## Comparison and Validation Options

### `--compare-stages`
Show diff between consecutive transformation stages.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --compare-stages
```

**Use case:** Understand what each normalization step changes.

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

### `--debug-format=FORMAT`
Set output format for debug information.

**Formats:**
- `pretty` - Multi-line pretty-printed (default)
- `compact` - Single-line compact format
- `json` - JSON output for tool integration
- `tree` - ASCII tree visualization

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-tokens --debug-format=compact
```

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

### `--memory-stats`
Show memory usage per stage (placeholder for future implementation).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --memory-stats
```

## Export and Logging Options

### `--debug-output-dir=DIR`
Write all debug outputs to directory instead of stdout.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-output-dir=debug-output
```

**Use case:** Preserve debug info for later analysis.

### `--debug-log=FILE`
Write detailed debug log with timestamps.

**Example:**
```bash
rtk test-grammars/java.rtk test-out --debug-log=debug.log
```

## Interactive Debug Mode

### `--interactive` / `-i`
Step through compilation stages interactively (placeholder for future implementation).

**Example:**
```bash
rtk test-grammars/java.rtk test-out --interactive
```

**Features:**
- Pause after each stage
- Inspect intermediate results
- Skip to specific stage
- Compare transformations

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
rtk grammar.rtk out --profile-stages --memory-stats
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

✅ Fully implemented:
- All pipeline stage inspection options
- Output stage inspection options
- Statistics and analysis options
- Validation options
- Output format options (pretty/compact)
- Performance profiling (timing)

🚧 Partial implementation:
- JSON and tree output formats (placeholders)
- Memory statistics (placeholder)
- Interactive mode (placeholder)
- Debug output directory export (placeholder)
- Debug logging (placeholder)

## See Also

- Main README for general RTK usage
- Grammar syntax documentation
- Examples in `test-grammars/` directory
