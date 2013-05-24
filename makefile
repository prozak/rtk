.PHONY: clean help test

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
BIN_PATH=dist/build/rtk/rtk
endif

SOURCES=$(wildcard *.hs *.x *.y)
build: $(BIN_PATH)

$(BIN_PATH): $(SOURCES)
	cabal build

clean:
	$(RM) $(RM_OPT) test-out
	cabal clean
	cabal configure

test:
	runhaskell StrQuote_Test.hs

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

%.hs : %.x
	alex $< -o $@

%.hs : %.y
	happy -ihappy_log.txt $< -o $@

test-out/grammar-main.hs: test-grammars/grammar-main.hs
	$(CP) test-grammars\grammar-main.hs test-out
	
test-grammar: build test-out test-out/grammar-main.hs test-out/GrammarLexer.hs test-out/GrammarParser.hs
	(cd test-out && ghc --make grammar-main.hs -o main)
	test-out/main test-grammars/grammar.pg

test-java: build test-out test-out/JavaLexer.hs test-out/JavaParser.hs
#	cp test-grammars/grammar-main.hs test-out
#	(cd test-out && ghc --make grammar-main.hs -o main)
#	test-out/main test-grammars/grammar.pg

test-t1: test-out build
	$(BIN_PATH) test-grammars/t1.pg test-out
