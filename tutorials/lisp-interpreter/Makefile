# Builds the lis.py-port Scheme interpreter against the RTK checkout two
# levels up. All Haskell tooling (rtk, alex, happy, and ghc with RTK's
# package set) is taken from the parent project via `cabal exec`, so build
# RTK first:
#
#     cd ../.. && cabal build
#
# Targets:
#     make build   - generate the front end from scheme.pg and build lis
#     make test    - run the interpreter's test suite and the examples
#     make repl    - build and start the REPL
#     make clean

ROOT := ../..
HERE := tutorials/lisp-interpreter
GEN  := gen

GENERATED := $(GEN)/SchemeLexer.hs $(GEN)/SchemeParser.hs $(GEN)/SchemeQQ.hs

.PHONY: all build test repl clean

all: build

build: lis

$(GEN)/SchemeLexer.x $(GEN)/SchemeParser.y $(GEN)/SchemeQQ.hs &: scheme.pg
	mkdir -p $(GEN)
	cd $(ROOT) && cabal exec rtk -- $(HERE)/scheme.pg $(HERE)/$(GEN)

$(GEN)/%.hs: $(GEN)/%.x
	cd $(ROOT) && cabal exec alex -- -g $(HERE)/$< -o $(HERE)/$@

$(GEN)/%.hs: $(GEN)/%.y
	cd $(ROOT) && cabal exec happy -- $(HERE)/$< --ghc -i$(HERE)/$(GEN)/$*.info -o $(HERE)/$@

lis: Main.hs $(GENERATED)
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/Main.hs -i$(HERE) -i$(HERE)/$(GEN) -outputdir $(HERE)/build/lis -o $(HERE)/lis

test: build
	./lis --test
	./lis examples/fact.scm
	./lis examples/fib.scm

repl: build
	./lis

clean:
	rm -rf $(GEN) build lis
