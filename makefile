.PHONY: clean help test $(GRAMMAR_TARGETS)

# Ensure PATH includes cabal binaries
export PATH := $(HOME)/.local/bin:$(PATH)

# Auto-discover grammar files
GRAMMARS := $(basename $(notdir $(wildcard test-grammars/*.pg)))
GRAMMAR_TARGETS := $(addprefix test-, $(GRAMMARS))

default: help

help:
	@echo "Use 'build' target to launch build"
	@echo "Use 'clean' target to clean binaries"
	@echo "Available grammar tests: $(GRAMMAR_TARGETS)"

ifeq ($(OS), Windows_NT)
CP=copy
RM=rmdir
RM_OPT=/s /q
MKDIR_P=mkdir
BIN_PATH=dist/build/rtk/rtk.exe
else
CP=cp
RM=rm
RM_OPT=-rf
MKDIR_P=mkdir -p
# Find the binary dynamically to support multiple platforms
BIN_PATH=$(shell find dist-newstyle -name rtk -type f -path '*/build/rtk/rtk' 2>/dev/null | head -n 1)
endif

SOURCES=$(wildcard *.hs *.x *.y)
build: $(BIN_PATH)

$(BIN_PATH): $(SOURCES)
	cabal build

clean:
	$(RM) $(RM_OPT) test-out
	cabal clean
	cabal configure

test: test-out build
	cabal exec ghc -- --make StrQuote_Test.hs -o test-out/strquote-test
	./test-out/strquote-test

test-out:
	$(MKDIR_P) test-out

# Function to capitalize first letter
capitalize = $(shell echo $(1) | awk '{print toupper(substr($$0,1,1)) tolower(substr($$0,2))}')

# Generic rule to generate lexer and parser from grammar files
define make-grammar-rule
test-out/$(call capitalize,$(1))Lexer.x test-out/$(call capitalize,$(1))Parser.y : $(BIN_PATH) test-grammars/$(1).pg
	$(BIN_PATH) test-grammars/$(1).pg test-out
endef

# Generate rules for each grammar
$(foreach grammar,$(GRAMMARS),$(eval $(call make-grammar-rule,$(grammar))))

%.hs : %.x
	cabal exec alex $< -- -o $@

%.hs : %.y
	cabal exec happy $< -- --ghc -ihappy_log.txt -o $@

# Generic rule to copy main files
test-out/%-main.hs: test-grammars/%-main.hs
	$(CP) test-grammars/$*-main.hs test-out

# Generic test rule - requires main file and test data to be defined
define make-test-rule
test-$(1): build test-out test-out/$(2)Lexer.hs test-out/$(2)Parser.hs test-out/$(1)-main.hs
	cabal exec -- ghc --make -itest-out test-out/$(1)-main.hs -o test-out/$(1)-main
	test-out/$(1)-main $(3)
endef

# Define test configurations: grammar-name, lexer-prefix, test-file
$(eval $(call make-test-rule,grammar,Grammar,test-grammars/grammar.pg))
$(eval $(call make-test-rule,java,Java,test-grammars/Test.java))
$(eval $(call make-test-rule,java-simple,JavaSimple,test-grammars/Simple.java))
$(eval $(call make-test-rule,sandbox,Sandbox,test-grammars/test.sandbox))
$(eval $(call make-test-rule,haskell,Haskell,Normalize.hs))
$(eval $(call make-test-rule,p,P,expr.p))

# Special cases that don't follow the pattern
test-t1: test-out build
	$(BIN_PATH) test-grammars/t1.pg test-out
