.phony: clean build help test-grammar test-ts

default: help

help:
	@echo "Use 'build' target to launch build"
	@echo "Use 'clean' target to clean binaries"
	@echo "Use 'test-grammar' target to generate [xy] for test-grammars/grammar.pg"
	@echo "Use 'test-t1' target to generate [xy] for test-grammars/t1.pg"

BIN_PATH=dist/build/rtk/rtk

build: $(BIN_PATH)

$(BIN_PATH): *.hs
	cabal build

clean:
	rm -rf test-out
	cabal clean
	cabal configure

test-out:
	mkdir -p test-out

test-grammar: build test-out 
	$(BIN_PATH) test-grammars/grammar.pg test-out
	(cd test-out && alex GrammarLexer.x)
	(cd test-out && happy GrammarParser.y)
	cp test-grammars/grammar-main.hs test-out
	(cd test-out && ghc --make grammar-main.hs -o main)
	test-out/main test-grammars/grammar.pg

test-java: build test-out 
	$(BIN_PATH) test-grammars/java.pg test-out
	(cd test-out && alex JavaLexer.x)
	(cd test-out && happy JavaParser.y)
#	cp test-grammars/grammar-main.hs test-out
#	(cd test-out && ghc --make grammar-main.hs -o main)
#	test-out/main test-grammars/grammar.pg

test-t1: test-out build
	$(BIN_PATH) test-grammars/t1.pg test-out
