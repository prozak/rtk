# RTK Self-Hosting Strategy

## Vision

Enable RTK to use its own generated parsers (from `test-grammars/grammar.pg`) instead of hand-written `Lexer.x` and `Parser.y`, while maintaining both paths during transition.

## Motivations

1. **Validation/Dogfooding**: Prove RTK is powerful enough to parse its own grammar
2. **Maintenance Simplification**: Edit grammar.pg instead of maintaining Alex/Happy files
3. **QuasiQuoter Features**: Enable compile-time grammar manipulation via GrammarQQ.hs
4. **Learning/Exploration**: Understand bootstrapping deeply

## Core Principles

1. **Incremental Progress**: Small experiments, frequent validation
2. **Dual-Mode Support**: Hand-written and generated parsers coexist
3. **QuasiQuotation First**: Ensure QQ works with generated AST
4. **Pragmatic Trade-offs**: Accept limitations (e.g., no nested comments) if semantic info preserved

## Key Decisions

### AST Structure
- **Accept different AST structure** from generated parser
- Use adapter pattern initially to convert between types
- Later: migrate codebase to use generated types directly
- **Critical**: Must support QuasiQuotation without exact type matching

### Nested Comments
- Simplify to regex-based approach in grammar.pg
- Document limitation
- Can add state machine support later if needed

### Error Handling
- Accept default Happy error messages initially
- Can enhance later if needed

## Prototype-Driven Roadmap

### Prototype 0: Baseline Discovery (30 mins)
**Goal:** See actual differences between hand-written and generated

**Actions:**
```bash
make test-grammar
make test-bootstrap
```

**Deliverable:** Gap analysis showing exact differences

---

### Prototype 1: Dual-Mode RTK (2-3 hours)
**Goal:** Add infrastructure to switch between parsers

**Changes:**
- Add `--use-generated` flag to main.hs
- Conditional imports based on mode
- Both modes compile (generated mode may not work yet)

**Testing:**
```bash
./rtk test-grammars/java.pg test-out                # hand-written mode
./rtk --use-generated test-grammars/java.pg test-out  # generated mode
```

---

### Prototype 2: Close the Loop (1 day)
**Goal:** Get generated parser to parse something successfully

**Approach:**
- Accept whatever AST RTK generates from grammar.pg
- Create adapter layer if needed
- Prove: generated parser can parse grammar.pg

**Success:** `./rtk --use-generated test-grammars/grammar.pg test-out` works

---

### Prototype 3: QuasiQuoter Validation (4-6 hours)
**Goal:** Ensure GrammarQQ.hs is usable

**Test:**
```haskell
import GrammarQQ
-- Write working examples using [rule| ... |] syntax
```

**Success:** At least one working QQ example that preserves semantic info

---

### Prototype 4: Bootstrap Cycle (1 day)
**Goal:** Prove fixed-point convergence

**Process:**
```
v1 (hand-written) → generates v2 parsers
v2 (generated) → generates v3 parsers
v2 ≡ v3 (fixed point!)
```

**Success:** v2 and v3 are functionally equivalent (diff -uwB passes)

---

### Prototype 5: Real Workload Test (2-3 days)
**Goal:** Parse all existing grammars with generated parser

**Test:** All test-grammars/*.pg parse successfully in both modes

**Success:** Full test suite passes with `--use-generated` flag

---

## Architecture

### File Structure
```
rtk/
├── Lexer.x                    # Hand-written (primary, stable)
├── Parser.y                   # Hand-written (primary, stable)
├── src/generated/
│   ├── GrammarLexer.x        # Generated (experimental)
│   ├── GrammarParser.y       # Generated (experimental)
│   └── GrammarQQ.hs          # Generated (QQ support)
├── main.hs                    # Dual-mode entry point
├── ASTAdapter.hs             # Convert generated ↔ hand-written AST
└── test-grammars/
    └── grammar.pg             # RTK's self-description
```

### Build Modes
```bash
# Default: use hand-written (stable)
cabal build

# Experimental: use generated
cabal build -f generated-parser
# OR
./rtk --use-generated <args>
```

## Success Metrics

### Phase 1: Proof of Concept
- ✅ Generated parser can parse grammar.pg
- ✅ GrammarQQ.hs compiles and is usable
- ✅ Bootstrap cycle reaches fixed point (v2 ≡ v3)

### Phase 2: Feature Parity
- ✅ All test-grammars/*.pg parse correctly
- ✅ Generated and hand-written produce equivalent output
- ✅ All existing tests pass in both modes

### Phase 3: Production Ready (Future)
- ✅ Generated mode is default
- ✅ Hand-written parsers archived as reference
- ✅ Documentation updated
- ✅ No regressions

## Risk Mitigation

**Risk:** Generated AST incompatible with existing code
**Mitigation:** Adapter pattern, incremental migration

**Risk:** Generated code has bugs
**Mitigation:** Comprehensive testing, dual-mode operation

**Risk:** Performance regression
**Mitigation:** Benchmarking, profiling

**Risk:** Lost features (nested comments, error messages)
**Mitigation:** Document trade-offs, can add back later if critical

## Timeline

- **Week 1**: Prototypes 0-2 (baseline + minimal bootstrap)
- **Week 2**: Prototypes 3-4 (QQ validation + bootstrap cycle)
- **Week 3**: Prototype 5 (real workload testing)
- **Week 4+**: Iteration based on findings

## Next Steps

1. Run `make test-bootstrap` to see current differences
2. Analyze gap and document findings
3. Implement Prototype 1 (dual-mode flag)
4. Iterate through prototypes
5. Re-evaluate strategy after each prototype

## Open Questions

- How much AST restructuring is acceptable?
- What's the bar for "good enough" QQ support?
- When do we make generated mode the default?
- Do we need custom semantic actions in grammar.pg?

## References

- BOOTSTRAP.md - Current test infrastructure
- compare-bootstrap.sh - Comparison script
- test-grammars/grammar.pg - RTK's self-description
