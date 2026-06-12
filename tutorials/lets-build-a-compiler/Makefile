# Builds the milestone-0 calculator against the RTK checkout two levels up.
# All Haskell tooling (rtk, alex, happy, and ghc with RTK's package set)
# is taken from the parent project via `cabal exec`, so build RTK first:
#
#     cd ../.. && cabal build
#
# Targets:
#     make build   - generate the front end from calc.pg and build test-qq
#     make test    - run the quasi-quotation test suite
#     make clean

ROOT := ../..
HERE := tutorials/lets-build-a-compiler
GEN  := gen

GENERATED := $(GEN)/CalcLexer.hs $(GEN)/CalcParser.hs $(GEN)/CalcQQ.hs

.PHONY: all build test clean

all: build

build: test-qq

$(GEN)/CalcLexer.x $(GEN)/CalcParser.y $(GEN)/CalcQQ.hs &: calc.pg
	mkdir -p $(GEN)
	cd $(ROOT) && cabal exec rtk -- $(HERE)/calc.pg $(HERE)/$(GEN)

$(GEN)/%.hs: $(GEN)/%.x
	cd $(ROOT) && cabal exec alex -- -g $(HERE)/$< -o $(HERE)/$@

$(GEN)/%.hs: $(GEN)/%.y
	cd $(ROOT) && cabal exec happy -- $(HERE)/$< --ghc -i$(HERE)/$(GEN)/$*.info -o $(HERE)/$@

test-qq: TestQQ.hs $(GENERATED)
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/TestQQ.hs -i$(HERE) -i$(HERE)/$(GEN) -outputdir $(HERE)/build/test-qq -o $(HERE)/test-qq

test: build
	./test-qq

clean:
	rm -rf $(GEN) build test-qq
