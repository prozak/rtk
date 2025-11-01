.PHONY: clean help test test-all-java test-bootstrap test-debug test-debug-all test-debug-options $(GRAMMAR_TARGETS)

# Ensure PATH includes cabal binaries
export PATH := $(HOME)/.local/bin:$(PATH)

# Auto-discover grammar files
GRAMMARS := $(basename $(notdir $(wildcard test-grammars/*.pg)))
GRAMMAR_TARGETS := $(addprefix test-, $(GRAMMARS))

default: help

help:
ifeq ($(OS), Windows_NT)
	@echo Use 'build' target to launch build
	@echo Use 'clean' target to clean binaries
	@echo Use 'test-grammar' target to generate [xy] for test-grammars/grammar.pg
	@echo Use 'test-t1' target to generate [xy] for test-grammars/t1.pg
	@echo Use 'test-bootstrap' target to compare hand-written vs generated grammar files
else
	@echo "Use 'build' target to launch build"
	@echo "Use 'clean' target to clean binaries"
	@echo "Use 'test-bootstrap' target to compare hand-written vs generated grammar files"
	@echo "Available grammar tests: $(GRAMMAR_TARGETS)"
endif

ifeq ($(OS), Windows_NT)
CP=copy
RM=rmdir
RM_OPT=/s /q
BIN_PATH=dist/build/rtk/rtk.exe
RTK_EXEC=$(BIN_PATH)
else
CP=cp
RM=rm
RM_OPT=-rf
BIN_PATH=dist-newstyle/build/aarch64-osx/ghc-9.12.2/rtk-0.10/x/rtk/build/rtk/rtk
RTK_EXEC=cabal exec rtk --
endif

SOURCES=$(wildcard *.hs *.x *.y)
build:
	cabal build

$(BIN_PATH): $(SOURCES)
	cabal build

clean:
	$(RM) $(RM_OPT) test-out
	cabal clean
	cabal configure

test: test-out build
	cabal exec ghc -- --make StrQuote_Test.hs -o test-out/strquote-test
	./test-out/strquote-test
	cabal exec ghc -- --make EmptyGrammar_Test.hs -o test-out/emptygrammar-test
	./test-out/emptygrammar-test

test-out:
ifeq ($(OS), Windows_NT)
	mkdir test-out
else
	mkdir -p test-out
endif

# Function to capitalize first letter
capitalize = $(shell echo $(1) | awk '{print toupper(substr($$0,1,1)) tolower(substr($$0,2))}')

# Generic rule to generate lexer and parser from grammar files
define make-grammar-rule
test-out/$(call capitalize,$(1))Lexer.x test-out/$(call capitalize,$(1))Parser.y : build test-grammars/$(1).pg
	$(RTK_EXEC) test-grammars/$(1).pg test-out
endef

# Generate rules for each grammar
$(foreach grammar,$(GRAMMARS),$(eval $(call make-grammar-rule,$(grammar))))

# Special cases for grammars with different naming patterns
test-out/JavaSimpleLexer.x test-out/JavaSimpleParser.y : build test-grammars/java-simple.pg
	$(RTK_EXEC) test-grammars/java-simple.pg test-out

%.hs : %.x
	cabal exec alex -- $< -o $@

%.hs : %.y
	cabal exec happy -- $< --ghc -ihappy_log.txt -o $@

# Generic rule to copy main files
test-out/%-main.hs: test-grammars/%-main.hs
	$(CP) test-grammars/$*-main.hs test-out

# Generic test rule - requires main file and test data to be defined
define make-test-rule
test-$(1): build test-out test-out/$(2)Lexer.hs test-out/$(2)Parser.hs test-out/$(1)-main.hs
	cabal exec -- ghc --make -itest-out test-out/$(1)-main.hs -o test-out/$(1)-main
	test-out/$(1)-main $(3)
endef

# Test rule for tests that share a main runner
# Parameters: test-name, shared-main-name, lexer-prefix, test-file
define make-shared-test-rule
test-$(1): build test-out test-out/$(3)Lexer.hs test-out/$(3)Parser.hs test-out/$(2)-main.hs test-out/$(2)-main
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

# Run all Java tests
test-all-java: test-java test-java-simple test-java-minimal test-java-field test-java-field-public test-java-package test-java-string test-java-complex test-java-full test-java-generics test-java-enum test-java-annotations test-java-empty-method test-java-simple-return test-java-return-field test-java-very-simple test-java-parameter-only test-java-field-this test-java-simple-assignment test-java-compound-assignment test-java-set-value
	@echo ""
	@echo "=== All Java tests completed successfully! ==="

# Special cases that don't follow the pattern
test-haskell: build test-out test-out/HaskellLexer.hs test-out/HaskellParser.hs
	$(CP) test-grammars/haskell-main.hs test-out
	(cd test-out && ghc --make haskell-main.hs -o haskell-rtk)
	test-out/haskell-rtk Normalize.hs

test-t1: test-out build
	$(RTK_EXEC) test-grammars/t1.pg test-out

test-p: test-out build test-out/PLexer.hs test-out/PParser.hs
	$(CP) test-grammars/p-main.hs test-out
	(cd test-out && ghc --make p-main.hs -o p-rtk)
	test-out/p-rtk expr.p

# Java quasi-quotation tests (separate from regular java-main parser driver)
test-java-qq: test-out build test-out/JavaLexer.hs test-out/JavaParser.hs
	$(CP) test-grammars/java-qq-test.hs test-out
	cabal exec -- ghc --make -itest-out test-out/java-qq-test.hs -o test-out/java-qq-test
	test-out/java-qq-test

# Bootstrap comparison test - compares hand-written files with generated ones
test-bootstrap: build test-out test-out/GrammarLexer.x test-out/GrammarParser.y test-out/GrammarQQ.hs
	@echo ""
	@echo "Running bootstrap comparison test..."
	./compare-bootstrap.sh

# Test debug options - uses grammar.pg as test subject
test-debug: test-out build
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
test-debug-all: test-out build
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
test-debug-options: test-out build
	@echo "========================================"
	@echo "Running automated debug options test suite"
	@echo "========================================"
	@./test-debug-options.sh
