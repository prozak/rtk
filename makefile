default: help

help:
	@echo "Use 'build' target to launch build"
	@echo "Use 'clean' target to clean binaries"
	@echo "Use 'test-grammar' target to generate [xy] for test-grammars/grammar.pg"
	@echo "Use 'test-t1' target to generate [xy] for test-grammars/t1.pg"

build:
	cabal build

clean:
	rm -rf test-out
	cabal clean
	cabal configure

.phony: clean build help test-grammar test-t1

test-out:
	mkdir -p test-out

test-grammar: test-out build
	./dist/build/rtk/rtk test-grammars/grammar.pg test-out
	(cd test-out && alex GrammarLexer.x)
	(cd test-out && happy GrammarParser.y)
	cp test-grammars/grammar-main.hs test-out
	(cd test-out && ghc --make grammar-main.hs -o main)
	test-out/main test-grammars/grammar.pg

test-t1: test-out build
	./dist/build/rtk/rtk test-grammars/t1.pg test-out
