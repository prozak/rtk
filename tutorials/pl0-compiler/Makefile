# Builds the PL/0 validator against the RTK checkout two levels up.
# All Haskell tooling (rtk, alex, happy, and ghc with RTK's package set)
# is taken from the parent project via `cabal exec`, so build RTK first:
#
#     cd ../.. && cabal build
#
# Targets:
#     make build   - generate front end from pl0.pg and build pl0 + test-qq
#     make test    - run the quasi-quotation tests and the parser tests
#     make clean

ROOT := ../..
HERE := tutorials/pl0-compiler
GEN  := gen

GENERATED := $(GEN)/Pl0Lexer.hs $(GEN)/Pl0Parser.hs $(GEN)/Pl0QQ.hs

.PHONY: all build test clean

all: build

build: pl0 test-qq

$(GEN)/Pl0Lexer.x $(GEN)/Pl0Parser.y $(GEN)/Pl0QQ.hs &: pl0.pg
	mkdir -p $(GEN)
	cd $(ROOT) && cabal exec rtk -- $(HERE)/pl0.pg $(HERE)/$(GEN)

$(GEN)/%.hs: $(GEN)/%.x
	cd $(ROOT) && cabal exec alex -- -g $(HERE)/$< -o $(HERE)/$@

$(GEN)/%.hs: $(GEN)/%.y
	cd $(ROOT) && cabal exec happy -- $(HERE)/$< --ghc -i$(HERE)/$(GEN)/$*.info -o $(HERE)/$@

pl0: Main.hs $(GENERATED)
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/Main.hs -i$(HERE) -i$(HERE)/$(GEN) -outputdir $(HERE)/build/pl0 -o $(HERE)/pl0

test-qq: TestQQ.hs $(GENERATED)
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/TestQQ.hs -i$(HERE) -i$(HERE)/$(GEN) -outputdir $(HERE)/build/test-qq -o $(HERE)/test-qq

test: build
	./test-qq
	./run_tests.sh

clean:
	rm -rf $(GEN) build pl0 test-qq
