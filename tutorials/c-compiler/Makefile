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

.PHONY: all build test conflict-check clean

all: build

build: ncc test-qq

# Pin the parsers' LALR conflict inventory (the C6 gotcha in the plan): c.pg
# owns exactly ONE shift/reduce conflict - the dangling else, resolved by
# happy's shift preference to bind the else to the nearest if - and asm.pg
# exactly one, the QQ bootstrap dummy-bracket conflict every generated
# grammar's whole-file entry carries (see java.pg's conflict family 6). A
# grammar change that adds conflicts can't hide behind either: this target
# fails the build the moment the counts move.
conflict-check: $(GEN)/CParser.hs $(GEN)/AsmParser.hs
	@c=$$(grep -oh "contains [0-9]* shift/reduce" $(GEN)/CParser.info | awk '{s+=$$2} END {print s+0}'); \
	a=$$(grep -oh "contains [0-9]* shift/reduce" $(GEN)/AsmParser.info | awk '{s+=$$2} END {print s+0}'); \
	rr=$$(grep -oh "contains [0-9]* reduce/reduce" $(GEN)/CParser.info $(GEN)/AsmParser.info | awk '{s+=$$2} END {print s+0}'); \
	if [ "$$c" != "1" ] || [ "$$a" != "1" ] || [ "$$rr" != "0" ]; then \
		echo "conflict-check: expected exactly 1 s/r in CParser (dangling else),"; \
		echo "  1 s/r in AsmParser (QQ dummy bracket), 0 r/r anywhere;"; \
		echo "  got CParser=$$c AsmParser=$$a reduce/reduce=$$rr"; \
		exit 1; \
	fi; \
	echo "conflict-check: dangling else + QQ dummy bracket, as pinned."

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

test: build conflict-check
	./test-qq
	./run_tests.sh

clean:
	rm -rf $(GEN) build ncc test-qq
