# Builds the stage-1 C compiler against the RTK checkout two levels up.
# All Haskell tooling (rtk, alex, happy, and ghc with RTK's package set)
# is taken from the parent project via `cabal exec`, so build RTK first:
#
#     cd ../.. && cabal build
#
# Targets:
#     make build   - generate front end from c.pg and build ncc + test-qq
#     make test    - run the quasi-quotation tests and the compiler tests
#     make clean

ROOT := ../..
HERE := tutorials/c-compiler
GEN  := gen

GENERATED := $(GEN)/CLexer.hs $(GEN)/CParser.hs $(GEN)/CQQ.hs \
             $(GEN)/AsmLexer.hs $(GEN)/AsmParser.hs $(GEN)/AsmQQ.hs

.PHONY: all build test clean

all: build

build: ncc test-qq

$(GEN)/CLexer.x $(GEN)/CParser.y $(GEN)/CQQ.hs &: c.pg
	mkdir -p $(GEN)
	cd $(ROOT) && cabal exec rtk -- $(HERE)/c.pg $(HERE)/$(GEN)

$(GEN)/AsmLexer.x $(GEN)/AsmParser.y $(GEN)/AsmQQ.hs &: asm.pg
	mkdir -p $(GEN)
	cd $(ROOT) && cabal exec rtk -- $(HERE)/asm.pg $(HERE)/$(GEN)

$(GEN)/%.hs: $(GEN)/%.x
	cd $(ROOT) && cabal exec alex -- -g $(HERE)/$< -o $(HERE)/$@

$(GEN)/%.hs: $(GEN)/%.y
	cd $(ROOT) && cabal exec happy -- $(HERE)/$< --ghc -i$(HERE)/$(GEN)/$*.info -o $(HERE)/$@

ncc: Main.hs Resolve.hs Codegen.hs Emit.hs $(GENERATED)
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/Main.hs -i$(HERE) -i$(HERE)/$(GEN) -outputdir $(HERE)/build/ncc -o $(HERE)/ncc

test-qq: TestQQ.hs Resolve.hs Codegen.hs Emit.hs $(GENERATED)
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/TestQQ.hs -i$(HERE) -i$(HERE)/$(GEN) -outputdir $(HERE)/build/test-qq -o $(HERE)/test-qq

test: build
	./test-qq
	./run_tests.sh

clean:
	rm -rf $(GEN) build ncc test-qq
