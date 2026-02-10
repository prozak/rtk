# RTK 2026 Roadmap

## Context

RTK went from a dormant 10-year-old project to a published v0.10 in 2025.
This document proposes the next steps for 2026.

## 2025 Recap

### Achievements
- **Project resurrection**: Restored to compilable state on GHC 9.4.7
- **Java grammar**: Comprehensive grammar with 69% conflict reduction (481→149)
- **CI/CD**: GitHub Actions with optimized caching (~2 min builds)
- **Testing**: 50+ test targets, Apache Commons Lang real-world suite (259+ files)
- **Quasi-quotation**: 26 construction tests, hierarchical splicing/pattern matching
- **Self-hosting foundation**: Prototypes 0-1 complete, dual-mode infrastructure
- **Publication prep**: MIT license, Hackage-ready v0.10, CHANGELOG

### Unfinished
- Self-hosting Prototypes 2-5 (generated parser doesn't work yet)
- QQ pattern matching and anti-quotation for Java
- GenY.hs: reverse operation and lifted rule TODOs
- Java 8+ features (lambdas, method references)
- Actual Hackage upload and git tags

---

## 2026 Priorities

### Priority 1: Complete Self-Hosting (Prototypes 2-4)

**Why**: This is the flagship feature that validates RTK's design. Prototypes 0-1 are
done; the hard work of closing the loop remains.

#### Prototype 2: Close the Loop
- Generate parser files from `grammar.pg` into `src/generated/`
- Compile generated `.x` and `.y` to Haskell via Alex/Happy
- Implement `ASTAdapter.hs` to convert generated AST → `InitialGrammar`
- Wire up `--use-generated` in `main.hs` to use generated lexer/parser
- Verify: `rtk --use-generated test-grammars/grammar.pg test-out` succeeds

#### Prototype 3: QuasiQuoter Validation
- Verify `GrammarQQ.hs` compiles and can be imported
- Write at least one working QQ example using generated grammar types
- Validate that QQ preserves semantic information

#### Prototype 4: Bootstrap Cycle
- Run: v1 (hand-written) → generates v2 → v2 generates v3
- Verify v2 ≡ v3 (fixed-point convergence)
- Document any semantic differences

**Success criteria**: `make test-bootstrap` shows convergence; `--use-generated`
mode passes all grammar tests.

---

### Priority 2: Hackage Publication

**Why**: The package is prepared but needs actual publication and proper versioning.

#### Tasks
- Tag v0.10 in git (`git tag v0.10`)
- Upload to Hackage (`cabal upload` / `cabal upload --publish`)
- Verify the package builds from Hackage (test in clean environment)
- Set up automated release workflow in CI
- Add Hackage badge to README

---

### Priority 3: QQ Pattern Matching & Anti-Quotation

**Why**: Construction-only QQ is useful but limited. Pattern matching would
enable powerful code analysis use cases.

#### Tasks
- Investigate why pattern matching doesn't work for Java QQ
- Implement anti-quotation support in GenQ.hs
- Add pattern matching test cases alongside existing 26 construction tests
- Document QQ capabilities and limitations

---

### Priority 4: Code Quality & Technical Debt

**Why**: Several small items accumulated during rapid 2025 development.

#### Tasks
- Resolve GenY.hs TODOs (reverse operation logic, lifted rule support)
- Add command-line options parsing to test driver main files
  (`haskell-main.hs`, `sandbox-main.hs`, `grammar-main.hs`)
- Evaluate whether `Debug.hs` (557 lines, largest module) should be split
- Consider extracting Token type to a separate module (needed for self-hosting)
- Remove or implement the `ASTAdapter.hs` scaffolding stub

---

### Priority 5: Java Grammar Enhancements

**Why**: The grammar handles Java 7 well but modern Java needs more coverage.

#### Tasks
- **Java 8**: Lambda expressions, method references, default methods, streams
- **Conflict reduction**: Further reduce the remaining 149 conflicts
- **Try-with-resources**: Enhanced exception handling syntax
- **Diamond operator**: Type inference for generics

---

### Priority 6: New Grammar Targets (Stretch)

**Why**: Proving RTK works beyond Java validates its generality.

#### Candidates
- **JSON**: Simple grammar, good for tutorials and documentation
- **SQL subset**: Practical and well-understood
- **Markdown subset**: Interesting parsing challenges
- **Haskell**: Already started (`haskell.pg` exists at 2,692 bytes),
  but needs completion and testing

---

## Proposed Milestone Plan

### Q1 2026: Self-Hosting & Publication
- Complete Prototypes 2-4
- Publish to Hackage
- Tag v0.10, plan v0.11

### Q2 2026: QQ & Quality
- QQ pattern matching & anti-quotation
- Technical debt cleanup (GenY.hs TODOs, module splitting)
- v0.11 release with self-hosting support

### Q3 2026: Java & Grammars
- Java 8+ grammar features
- New grammar target (JSON or SQL)
- v0.12 release

### Q4 2026: Polish & Growth
- Documentation improvements
- Tutorial/examples for new users
- Evaluate community feedback from Hackage
- Plan v1.0 criteria

---

## Open Questions

1. **Should self-hosting become the default?** Or remain opt-in via `--use-generated`?
2. **Is Hackage the right distribution channel?** Or should we focus on GitHub releases?
3. **How much Java coverage is enough?** Full Java 17? Or is Java 7-8 sufficient?
4. **Should RTK support incremental parsing?** (Significant architectural change)
5. **Is there demand for error recovery?** Currently uses default Happy error handling.

---

## Risk Assessment

| Risk | Impact | Mitigation |
|------|--------|------------|
| Self-hosting AST adapter too complex | High | Accept pragmatic adapter, migrate incrementally |
| Hackage package rejected | Medium | Test with `cabal check`, fix warnings |
| Java 8 lambdas cause grammar explosions | Medium | Start with subset, measure conflicts |
| QQ pattern matching fundamentally limited | Medium | Document limitations, focus on construction |
| GHC version compatibility | Low | Test with GHC 9.6/9.8, update cabal bounds |
