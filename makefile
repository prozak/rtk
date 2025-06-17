.PHONY: clean help test

# Ensure PATH includes cabal binaries
export PATH := $(HOME)/.local/bin:$(PATH)

default: help

help:
ifeq ($(OS), Windows_NT)
	@echo Use 'build' target to launch build
	@echo Use 'clean' target to clean binaries
	@echo Use 'test-grammar' target to generate [xy] for test-grammars/grammar.pg
	@echo Use 'test-t1' target to generate [xy] for test-grammars/t1.pg
else
	@echo "Use 'build' target to launch build"
	@echo "Use 'clean' target to clean binaries"
	@echo "Use 'test-grammar' target to generate [xy] for test-grammars/grammar.pg"
	@echo "Use 'test-java-simple' target to test simple Java syntax parsing"
	@echo "Use 'test-java' target to test full Java syntax parsing"
	@echo "Use 'test-t1' target to generate [xy] for test-grammars/t1.pg"
endif

ifeq ($(OS), Windows_NT)
CP=copy
RM=rmdir
RM_OPT=/s /q
BIN_PATH=dist/build/rtk/rtk.exe
else
CP=cp
RM=rm
RM_OPT=-rf
BIN_PATH=dist-newstyle/build/aarch64-osx/ghc-9.12.2/rtk-0.10/x/rtk/build/rtk/rtk
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
ifeq ($(OS), Windows_NT)
	mkdir test-out
else
	mkdir -p test-out
endif

test-out/GrammarLexer.x test-out/GrammarParser.y : $(BIN_PATH) test-grammars/grammar.pg
	$(BIN_PATH) test-grammars/grammar.pg test-out

test-out/JavaLexer.x test-out/JavaParser.y : $(BIN_PATH) test-grammars/java.pg
	$(BIN_PATH) test-grammars/java.pg test-out

test-out/JavaSimpleLexer.x test-out/JavaSimpleParser.y : $(BIN_PATH) test-grammars/java-simple.pg
	$(BIN_PATH) test-grammars/java-simple.pg test-out

test-out/HaskellLexer.x test-out/HaskellParser.y : $(BIN_PATH) test-grammars/haskell.pg
	$(BIN_PATH) test-grammars/haskell.pg test-out

test-out/PLexer.x test-out/PParser.y : $(BIN_PATH) test-grammars/p.pg
	$(BIN_PATH) test-grammars/p.pg test-out

%.hs : %.x
	cabal exec alex $< -- -o $@

%.hs : %.y
	cabal exec happy $< -- --ghc -ihappy_log.txt -o $@

test-out/grammar-main.hs: test-grammars/grammar-main.hs
	$(CP) test-grammars/grammar-main.hs test-out
	
test-grammar: build test-out test-out/grammar-main.hs test-out/GrammarLexer.hs test-out/GrammarParser.hs
	cabal exec -- ghc --make -itest-out test-out/grammar-main.hs -o test-out/main
	test-out/main test-grammars/grammar.pg

test-out/java-main.hs: test-grammars/java-main.hs
	$(CP) test-grammars/java-main.hs test-out

test-out/java-simple-main.hs: test-grammars/java-simple-main.hs
	$(CP) test-grammars/java-simple-main.hs test-out

test-java-simple: build test-out test-out/JavaSimpleLexer.hs test-out/JavaSimpleParser.hs test-out/java-simple-main.hs
	cabal exec -- ghc -O0 -w --make -itest-out test-out/java-simple-main.hs -o test-out/java-simple-main
	test-out/java-simple-main test-grammars/Simple.java

test-java: build test-out test-out/JavaLexer.hs test-out/JavaParser.hs test-out/java-main.hs
	cabal exec -- ghc --make -itest-out test-out/java-main.hs -o test-out/java-main
	test-out/java-main test-grammars/Test.java

test-haskell: build test-out test-out/HaskellLexer.hs test-out/HaskellParser.hs
	$(CP) test-grammars/haskell-main.hs test-out
	(cd test-out && ghc --make haskell-main.hs -o haskell-rtk)
	test-out/haskell-rtk Normalize.hs

test-t1: test-out build
	$(BIN_PATH) test-grammars/t1.pg test-out

test-p: test-out build test-out/PLexer.hs test-out/PParser.hs
	$(CP) test-grammars/p-main.hs test-out
	(cd test-out && ghc --make p-main.hs -o p-rtk)
	test-out/p-rtk expr.p
