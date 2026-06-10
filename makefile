.PHONY: clean help test test-unit test-golden accept-golden test-compile-goldens repl-lc repl-stlc repl-poly repl-proto test-lex-java accept-lex-java test-all-java test-i14-qq test-debug test-debug-all test-debug-options test-suite-commons-lang test-suite-commons-lang-tests test-suite-commons-lang-all test-lex-commons-lang test-lex-commons-lang-tests test-lex-commons-lang-all test-parse-commons-lang test-parse-commons-lang-tests test-parse-commons-lang-all analyze-failures test-suite $(GRAMMAR_TARGETS)

# ============================================================================
# Configuration
# ============================================================================

# Ensure PATH includes cabal binaries
export PATH := $(HOME)/.local/bin:$(PATH)

# Auto-discover grammar files
GRAMMARS := $(basename $(notdir $(wildcard test-grammars/*.pg)))
GRAMMAR_TARGETS := $(addprefix test-, $(GRAMMARS))

# ============================================================================
# Default target and help
# ============================================================================

default: help

help:
	@echo "Use 'build' target to launch build"
	@echo "Use 'clean' target to clean binaries"
	@echo "Use 'test' target to run the cabal test suites (unit + golden)"
	@echo "Use 'accept-golden' target to refresh golden snapshots after generator changes"
	@echo "Use 'test-lex-java' target to check exact Java token streams against goldens"
	@echo "Use 'accept-lex-java' target to refresh the Java token goldens after a lexer change"
	@echo "Available grammar tests: $(GRAMMAR_TARGETS)"

# ============================================================================
# Tool configuration
# ============================================================================
# (A Windows_NT branch used to live here, pointing at the long-dead cabal-v1
# dist/build path; this makefile supports Unix-like systems only.)

CP=cp
RM=rm
RM_OPT=-rf
MKDIR_P=mkdir -p
# Find the binary dynamically to support multiple platforms
BIN_PATH=$(shell find dist-newstyle -name rtk -type f -path '*/build/rtk/rtk' 2>/dev/null | head -n 1)
RTK_EXEC=cabal exec rtk --

# ============================================================================
# Build targets
# ============================================================================

SOURCES=$(wildcard *.hs *.x *.y app/*.hs)
build:
	cabal build

$(BIN_PATH): $(SOURCES)
	cabal build

# ============================================================================
# Clean and test targets
# ============================================================================

clean:
	$(RM) $(RM_OPT) test-out
	cabal clean
	cabal configure

# Fast in-process test suites (no alex/happy/GHC compile cycle):
#   unit   - HUnit tests incl. normalization invariants for every test grammar
#   golden - generated .x/.y/QQ.hs output compared against test/golden/ snapshots
test: test-unit test-golden

test-unit:
	cabal test unit --test-show-details=direct

test-golden:
	cabal test golden --test-show-details=direct

# Regenerate the golden snapshots after an intentional generator change,
# then review the diff of test/golden/ like any other code change.
accept-golden:
	RTK_ACCEPT=1 cabal test golden --test-show-details=direct

# Compile gate for the golden snapshots: run alex and happy over every
# checked-in test/golden/<grammar>/ Lexer.x/Parser.y pair and typecheck the
# result with GHC (-fno-code, no object code, seconds per grammar). The
# golden suite compares text only; this target proves the snapshots are code
# GHC accepts (the debug-test snapshot was uncompilable for a while with CI
# green - issue #34). The <Name>QQ.hs goldens are gated too (task 8b): the
# generated quasi-quoter needs only syb, containers and template-haskell,
# all of which rtk's own environment provides.
test-compile-goldens: build | test-out
	@set -e; \
	for dir in test/golden/*/; do \
		g=$$(basename "$$dir"); \
		out="test-out/compile-goldens/$$g"; \
		$(MKDIR_P) "$$out"; \
		echo "=== compiling golden snapshot: $$g"; \
		for x in "$$dir"*.x; do \
			cabal exec alex -- -g "$$x" -o "$$out/$$(basename "$${x%.x}").hs"; \
		done; \
		for y in "$$dir"*.y; do \
			cabal exec happy -- "$$y" --ghc -o "$$out/$$(basename "$${y%.y}").hs"; \
		done; \
		for y in "$$dir"*.y; do \
			cabal exec -- ghc -fno-code -w -i"$$out" "$$out/$$(basename "$${y%.y}").hs"; \
		done; \
		for q in "$$dir"*QQ.hs; do \
			cabal exec -- ghc -fno-code -w -i"$$out" -i"$$dir" "$$q"; \
		done; \
	done; \
	echo "All golden Lexer/Parser/QQ snapshots compile."

test-out:
	$(MKDIR_P) test-out

# ============================================================================
# Grammar generation rules
# ============================================================================

# Function to capitalize first letter and handle hyphenated names (e.g., java-simple → JavaSimple)
capitalize = $(shell echo $(1) | awk -F'-' '{for(i=1;i<=NF;i++) $$i=toupper(substr($$i,1,1)) substr($$i,2); print}' OFS='')

# Generic rule to generate lexer and parser from grammar files
define make-grammar-rule
test-out/$(call capitalize,$(1))Lexer.x test-out/$(call capitalize,$(1))Parser.y : test-grammars/$(1).pg | build test-out
	$(RTK_EXEC) test-grammars/$(1).pg test-out
endef

# Generate rules for each grammar
$(foreach grammar,$(GRAMMARS),$(eval $(call make-grammar-rule,$(grammar))))

%.hs : %.x
	cabal exec alex -- -g $< -o $@

%.hs : %.y
	cabal exec happy -- $< --ghc -ihappy_log.txt -o $@

# ============================================================================
# Test execution rules
# ============================================================================

# Generic rule to copy main files
test-out/%-main.hs: test-grammars/%-main.hs | test-out
	$(CP) test-grammars/$*-main.hs test-out

# Generic test rule - requires main file and test data to be defined
define make-test-rule
test-$(1): build test-out/$(2)Lexer.hs test-out/$(2)Parser.hs test-out/$(1)-main.hs | test-out
	cabal exec -- ghc --make -itest-out test-out/$(1)-main.hs -o test-out/$(1)-main
	test-out/$(1)-main $(3)
endef

# Test rule for tests that share a main runner
# Parameters: test-name, shared-main-name, lexer-prefix, test-file
define make-shared-test-rule
test-$(1): build test-out/$(3)Lexer.hs test-out/$(3)Parser.hs test-out/$(2)-main.hs test-out/$(2)-main | test-out
	test-out/$(2)-main $(4)
endef

# Shared main binary build rule (defined once, used by multiple tests)
test-out/java-main: test-out/java-main.hs test-out/JavaLexer.hs test-out/JavaParser.hs
	cabal exec -- ghc --make -itest-out test-out/java-main.hs -o test-out/java-main

# Define test configurations: grammar-name, lexer-prefix, test-file
$(eval $(call make-test-rule,grammar,Grammar,test-grammars/grammar.pg))
$(eval $(call make-test-rule,java,Java,test-grammars/TestBasic.java))
$(eval $(call make-test-rule,java-simple,JavaSimple,test-grammars/Simple.java))
$(eval $(call make-test-rule,sandbox,Sandbox,test-grammars/test.sandbox))
$(eval $(call make-test-rule,haskell,Haskell,Normalize.hs))
$(eval $(call make-test-rule,p,P,expr.p))

# Additional Java tests using the Java grammar (java.pg) - all share java-main runner
$(eval $(call make-shared-test-rule,java-minimal,java,Java,test-grammars/java/test-minimal.java))
$(eval $(call make-shared-test-rule,java-field-public,java,Java,test-grammars/java/test-field-public.java))
$(eval $(call make-shared-test-rule,java-package,java,Java,test-grammars/java/test-package.java))
$(eval $(call make-shared-test-rule,java-string,java,Java,test-grammars/java/test-simple-string.java))
$(eval $(call make-shared-test-rule,java-complex,java,Java,test-grammars/Complex.java))
$(eval $(call make-shared-test-rule,java-full,java,Java,test-grammars/Test.java))
$(eval $(call make-shared-test-rule,java-generics,java,Java,test-grammars/TestGenerics.java))
$(eval $(call make-shared-test-rule,java-enum,java,Java,test-grammars/TestEnum.java))
$(eval $(call make-shared-test-rule,java-annotations,java,Java,test-grammars/TestAnnotations.java))
$(eval $(call make-shared-test-rule,java-field,java,Java,test-grammars/java/test-field.java))
$(eval $(call make-shared-test-rule,java-empty-method,java,Java,test-grammars/java/test-empty-method.java))
$(eval $(call make-shared-test-rule,java-simple-return,java,Java,test-grammars/java/test-simple-return.java))
$(eval $(call make-shared-test-rule,java-return-field,java,Java,test-grammars/java/test-return-field.java))
$(eval $(call make-shared-test-rule,java-very-simple,java,Java,test-grammars/java/test-very-simple.java))
$(eval $(call make-shared-test-rule,java-parameter-only,java,Java,test-grammars/java/test-parameter-only.java))
$(eval $(call make-shared-test-rule,java-field-this,java,Java,test-grammars/java/test-field-this.java))
$(eval $(call make-shared-test-rule,java-simple-assignment,java,Java,test-grammars/java/test-simple-assignment.java))
$(eval $(call make-shared-test-rule,java-compound-assignment,java,Java,test-grammars/java/test-compound-assignment.java))
$(eval $(call make-shared-test-rule,java-set-value,java,Java,test-grammars/java/test-set-value.java))
$(eval $(call make-shared-test-rule,java-implements,java,Java,test-grammars/java/test-implements.java))
$(eval $(call make-shared-test-rule,java-nested-if,java,Java,test-grammars/java/test-nested-if.java))

# JavaDoc comment tests (blank line + {@link Class#method()} regression tests)
$(eval $(call make-shared-test-rule,java-javadoc-blank-link,java,Java,test-grammars/java/javadoc/test-blank-then-link.java))
$(eval $(call make-shared-test-rule,java-javadoc-minimal-hash,java,Java,test-grammars/java/javadoc/test-minimal-hash.java))
$(eval $(call make-shared-test-rule,java-javadoc-minimal-fail,java,Java,test-grammars/java/javadoc/test-minimal-fail.java))
$(eval $(call make-shared-test-rule,java-javadoc-link-tag,java,Java,test-grammars/java/javadoc/test-link-tag.java))
$(eval $(call make-shared-test-rule,java-javadoc-just-hash,java,Java,test-grammars/java/javadoc/test-just-hash.java))

# Java lexer golden tests: exact token streams for the lexical corpus in
# test-grammars/java/lexical/ are compared against the .tokens golden files.
# Catches mis-tokenization that --lex-only cannot (wrong/split tokens).
test-lex-java: test-out/java-main
	./test-java-lexical.sh

# Regenerate the .tokens golden files after an intentional lexer change,
# then review the diff like any other code change.
accept-lex-java: test-out/java-main
	ACCEPT=1 ./test-java-lexical.sh

# Run all Java tests
test-all-java: test-java test-java-simple test-java-minimal test-java-field test-java-field-public test-java-package test-java-string test-java-complex test-java-full test-java-generics test-java-enum test-java-annotations test-java-empty-method test-java-simple-return test-java-return-field test-java-very-simple test-java-parameter-only test-java-field-this test-java-simple-assignment test-java-compound-assignment test-java-set-value test-java-implements test-java-nested-if test-java-javadoc-blank-link test-java-javadoc-minimal-hash test-java-javadoc-minimal-fail test-java-javadoc-link-tag test-java-javadoc-just-hash
	@echo ""
	@echo "=== All Java tests completed successfully! ==="

# Special cases that don't follow the pattern
test-t1: build | test-out
	$(RTK_EXEC) test-grammars/t1.pg test-out

# Untyped lambda calculus interpreter generated from lc.pg (Write You a
# Haskell, chapters 3-4; see docs/write-you-a-haskell.md): QQ-driven
# evaluator, substitution and pretty-printer, with a test suite and REPL
test-out/lc-main: test-out/lc-main.hs test-out/LcLexer.hs test-out/LcParser.hs
	cabal exec -- ghc --make -itest-out test-out/lc-main.hs -o test-out/lc-main

test-lc: build test-out/lc-main | test-out
	test-out/lc-main

repl-lc: build test-out/lc-main | test-out
	test-out/lc-main repl

# Simply typed lambda calculus generated from stlc.pg (Write You a Haskell,
# chapters 5-6): QQ-driven typechecker plus a strategy-parameterized
# evaluator, with a test suite and a typecheck-then-eval REPL
test-out/stlc-main: test-out/stlc-main.hs test-out/StlcLexer.hs test-out/StlcParser.hs
	cabal exec -- ghc --make -itest-out test-out/stlc-main.hs -o test-out/stlc-main

test-stlc: build test-out/stlc-main | test-out
	test-out/stlc-main

repl-stlc: build test-out/stlc-main | test-out
	test-out/stlc-main repl

# Poly generated from poly.pg (Write You a Haskell, chapter 7):
# desugaring as QQ rewrites, Hindley-Milner inference (algorithm W),
# call-by-value evaluation with fix, and a stateful REPL
test-out/poly-main: test-out/poly-main.hs test-out/PolyLexer.hs test-out/PolyParser.hs
	cabal exec -- ghc --make -itest-out test-out/poly-main.hs -o test-out/poly-main

test-poly: build test-out/poly-main | test-out
	test-out/poly-main

repl-poly: build test-out/poly-main | test-out
	test-out/poly-main repl

# ProtoHaskell-lite generated from proto.pg (Write You a Haskell,
# chapters 8-12): algebraic data types, case with nested patterns,
# renamer, inference and evaluation; explicit { ; } blocks (layout
# support for RTK is tracked in github issue #95)
test-out/proto-main: test-out/proto-main.hs test-out/ProtoLexer.hs test-out/ProtoParser.hs
	cabal exec -- ghc --make -itest-out test-out/proto-main.hs -o test-out/proto-main

test-proto: build test-out/proto-main | test-out
	test-out/proto-main

repl-proto: build test-out/proto-main | test-out
	test-out/proto-main repl

# Java quasi-quotation tests (separate from regular java-main parser driver)
test-java-qq: build test-out/JavaLexer.hs test-out/JavaParser.hs | test-out
	$(CP) test-grammars/java-qq-test.hs test-out
	cabal exec -- ghc --make -itest-out test-out/java-qq-test.hs -o test-out/java-qq-test
	test-out/java-qq-test

# Quasi-quotation against covered pure types (issue #14): i14.pg's 'Shape'
# and 'Label' exist only through rule annotations; the synthesized cover
# rules give them top-level quoters and $Type:var splices like any
# hand-written type-named rule.
test-i14-qq: build test-out/I14Lexer.hs test-out/I14Parser.hs | test-out
	$(CP) test-grammars/i14-qq-test.hs test-out
	cabal exec -- ghc --make -itest-out test-out/i14-qq-test.hs -o test-out/i14-qq-test
	test-out/i14-qq-test

# Test debug options - uses grammar.pg as test subject
test-debug: build | test-out
	@echo "========================================"
	@echo "Testing RTK Debug Options"
	@echo "========================================"
	@echo ""
	@echo ">>> Testing --stats option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --stats
	@echo ""
	@echo ">>> Testing --debug-tokens option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --debug-tokens | head -20
	@echo ""
	@echo ">>> Testing --debug-parse option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --debug-parse | head -20
	@echo ""
	@echo ">>> Testing --list-rules option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --list-rules
	@echo ""
	@echo ">>> Testing --validate-grammar option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --validate-grammar
	@echo ""
	@echo ">>> Testing --show-rule-graph option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --show-rule-graph
	@echo ""
	@echo ">>> Testing --analyze-conflicts option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --analyze-conflicts
	@echo ""
	@echo ">>> Testing --unused-rules option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --unused-rules
	@echo ""
	@echo ">>> Testing --check-left-recursion option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --check-left-recursion
	@echo ""
	@echo ">>> Testing --suggest-shortcuts option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --suggest-shortcuts
	@echo ""
	@echo ">>> Testing --profile-stages option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --profile-stages
	@echo ""
	@echo ">>> Testing --debug-parser-spec option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --debug-parser-spec | head -30
	@echo ""
	@echo ">>> Testing --debug-constructors option"
	$(RTK_EXEC) test-grammars/grammar.pg test-out --debug-constructors | head -30
	@echo ""
	@echo "========================================"
	@echo "All debug option tests completed!"
	@echo "========================================"

# Comprehensive test of all debug options with java-simple.pg
test-debug-all: build | test-out
	@echo "========================================"
	@echo "Comprehensive Debug Options Test"
	@echo "Using java-simple.pg grammar"
	@echo "========================================"
	@echo ""
	@echo "=== Pipeline Stage Inspection ==="
	@echo ">>> --debug-tokens"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-tokens > test-out/debug-tokens.txt
	@echo "    Output saved to test-out/debug-tokens.txt"
	@echo ">>> --debug-parse"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-parse > test-out/debug-parse.txt
	@echo "    Output saved to test-out/debug-parse.txt"
	@echo ">>> --debug-string-norm"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-string-norm > test-out/debug-string-norm.txt
	@echo "    Output saved to test-out/debug-string-norm.txt"
	@echo ">>> --debug-clause-norm"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-clause-norm > test-out/debug-clause-norm.txt
	@echo "    Output saved to test-out/debug-clause-norm.txt"
	@echo ">>> --debug-constructors"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-constructors > test-out/debug-constructors.txt
	@echo "    Output saved to test-out/debug-constructors.txt"
	@echo ""
	@echo "=== Output Inspection ==="
	@echo ">>> --debug-parser-spec"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-parser-spec > test-out/debug-parser-spec.txt
	@echo "    Output saved to test-out/debug-parser-spec.txt"
	@echo ">>> --debug-lexer-spec"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-lexer-spec > test-out/debug-lexer-spec.txt
	@echo "    Output saved to test-out/debug-lexer-spec.txt"
	@echo ">>> --debug-qq-spec"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --debug-qq-spec > test-out/debug-qq-spec.txt
	@echo "    Output saved to test-out/debug-qq-spec.txt"
	@echo ""
	@echo "=== Statistics and Analysis ==="
	@echo ">>> --stats"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --stats | tee test-out/debug-stats.txt
	@echo ">>> --analyze-conflicts"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --analyze-conflicts | tee test-out/debug-conflicts.txt
	@echo ">>> --show-rule-graph"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --show-rule-graph > test-out/debug-rule-graph.txt
	@echo "    Output saved to test-out/debug-rule-graph.txt"
	@echo ">>> --list-rules"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --list-rules > test-out/debug-list-rules.txt
	@echo "    Output saved to test-out/debug-list-rules.txt"
	@echo ""
	@echo "=== Validation ==="
	@echo ">>> --validate-grammar"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --validate-grammar | tee test-out/debug-validate.txt
	@echo ">>> --unused-rules"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --unused-rules | tee test-out/debug-unused.txt
	@echo ">>> --check-left-recursion"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --check-left-recursion | tee test-out/debug-left-rec.txt
	@echo ">>> --suggest-shortcuts"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --suggest-shortcuts | tee test-out/debug-shortcuts.txt
	@echo ""
	@echo "=== Performance Profiling ==="
	@echo ">>> --profile-stages"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --profile-stages | tee test-out/debug-profile.txt
	@echo ""
	@echo "=== Combined Options ==="
	@echo ">>> --stats --validate-grammar --profile-stages"
	$(RTK_EXEC) test-grammars/java-simple.pg test-out --stats --validate-grammar --profile-stages | tee test-out/debug-combined.txt
	@echo ""
	@echo "========================================"
	@echo "All comprehensive debug tests completed!"
	@echo "Debug outputs saved in test-out/debug-*.txt"
	@echo "========================================"

# Automated test suite for all debug options
test-debug-options: build | test-out
	@echo "========================================"
	@echo "Running automated debug options test suite"
	@echo "========================================"
	@./test-debug-options.sh

# ============================================================================
# Java Test Suite (external codebases)
# ============================================================================

# Test Apache Commons Lang main sources (259 files) - Full parsing (informational)
test-suite-commons-lang: build
	@echo "========================================"
	@echo "Testing Apache Commons Lang (main sources)"
	@echo "========================================"
	@./test-java-suite.sh test-suites/commons-lang/src/main/java test-results/commons-lang-main

# Test Apache Commons Lang test sources (267 files) - Full parsing (informational)
test-suite-commons-lang-tests: build
	@echo "========================================"
	@echo "Testing Apache Commons Lang (test sources)"
	@echo "========================================"
	@./test-java-suite.sh test-suites/commons-lang/src/test/java test-results/commons-lang-tests

# Test both main and test sources - Full parsing (informational)
test-suite-commons-lang-all: test-suite-commons-lang test-suite-commons-lang-tests
	@echo ""
	@echo "========================================"
	@echo "Apache Commons Lang complete test suite finished"
	@echo "========================================"

# Lexical parsing tests for Apache Commons Lang (required tests, will fail on errors)
# Uses RTK-generated Alex lexer (escape sequence fix enables proper comment handling)
test-lex-commons-lang: test-out/java-main
	@echo "========================================"
	@echo "Lexical Parsing: Apache Commons Lang (main sources)"
	@echo "This is a REQUIRED test - failures will break the build"
	@echo "========================================"
	@JAVA_PARSER=./test-out/java-main ./test-java-suite.sh --lex-only --blacklist test-suites/commons-lang-lexer-blacklist.txt test-suites/commons-lang/src/main/java test-results/commons-lang-lex-main

test-lex-commons-lang-tests: test-out/java-main
	@echo "========================================"
	@echo "Lexical Parsing: Apache Commons Lang (test sources)"
	@echo "This is a REQUIRED test - failures will break the build"
	@echo "========================================"
	@JAVA_PARSER=./test-out/java-main ./test-java-suite.sh --lex-only --blacklist test-suites/commons-lang-lexer-blacklist-tests.txt test-suites/commons-lang/src/test/java test-results/commons-lang-lex-tests

# Test both main and test sources with lexical parsing only
test-lex-commons-lang-all: test-lex-commons-lang test-lex-commons-lang-tests
	@echo ""
	@echo "========================================"
	@echo "Apache Commons Lang lexical parsing tests completed"
	@echo "========================================"

# Full parsing tests for Apache Commons Lang (required tests, will fail on errors)
# Uses RTK-generated Happy parser with blacklist for unsupported Java 8+ features
test-parse-commons-lang: test-out/java-main
	@echo "========================================"
	@echo "Full Parsing: Apache Commons Lang (main sources)"
	@echo "This is a REQUIRED test - failures will break the build"
	@echo "========================================"
	@JAVA_PARSER=./test-out/java-main ./test-java-suite.sh --blacklist test-suites/commons-lang-parser-blacklist.txt test-suites/commons-lang/src/main/java test-results/commons-lang-parse-main

test-parse-commons-lang-tests: test-out/java-main
	@echo "========================================"
	@echo "Full Parsing: Apache Commons Lang (test sources)"
	@echo "This is a REQUIRED test - failures will break the build"
	@echo "========================================"
	@JAVA_PARSER=./test-out/java-main ./test-java-suite.sh --blacklist test-suites/commons-lang-parser-blacklist-tests.txt test-suites/commons-lang/src/test/java test-results/commons-lang-parse-tests

# Test both main and test sources with full parsing
test-parse-commons-lang-all: test-parse-commons-lang test-parse-commons-lang-tests
	@echo ""
	@echo "========================================"
	@echo "Apache Commons Lang full parsing tests completed"
	@echo "========================================"

# Analyze failure patterns from most recent test run
analyze-failures:
	@if [ -z "$(DIR)" ]; then \
		LATEST=$$(ls -td test-results/*/ 2>/dev/null | head -1); \
		if [ -z "$$LATEST" ]; then \
			echo "No test results found. Run a test suite first."; \
			exit 1; \
		fi; \
		echo "Analyzing latest results: $$LATEST"; \
		./analyze-failures.sh "$$LATEST"; \
	else \
		./analyze-failures.sh "$(DIR)"; \
	fi

# Quick test on a single directory
test-suite: build
	@if [ -z "$(DIR)" ]; then \
		echo "Usage: make test-suite DIR=<path-to-java-sources>"; \
		echo "Example: make test-suite DIR=test-suites/commons-lang/src/main/java"; \
		exit 1; \
	fi
	@./test-java-suite.sh "$(DIR)" "test-results/$$(basename $(DIR))-$$(date +%Y%m%d-%H%M%S)"


