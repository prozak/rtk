# Builds the Write You a Haskell interpreters against the RTK checkout two
# levels up. All Haskell tooling (rtk, alex, happy, and ghc with RTK's
# package set) is taken from the parent project via `cabal exec`, so build
# RTK first:
#
#     cd ../.. && cabal build
#
# Targets:
#     make build       - generate all four front ends and build the binaries
#     make test        - run every language's test suite
#     make test-<lang> - one language's suite   (lc, stlc, poly, proto)
#     make repl-<lang> - interactive REPL       (lc, stlc, poly, proto)
#     make clean

ROOT := ../..
HERE := tutorials/write-you-a-haskell
GEN  := gen

LANGS := lc stlc poly proto

.PHONY: all build test clean $(addprefix test-,$(LANGS)) $(addprefix repl-,$(LANGS))

all: build

build: $(LANGS)

test: build
	./lc
	./stlc
	./poly
	./proto

# One language: generate the front end from the grammar with rtk, then
# compile the driver against it. $(1) = grammar/driver/binary name,
# $(2) = capitalized name used in the generated module names.
define lang-rules
$(GEN)/$(2)Lexer.x $(GEN)/$(2)Parser.y $(GEN)/$(2)QQ.hs &: $(1).pg
	mkdir -p $(GEN)
	cd $(ROOT) && cabal exec rtk -- $(HERE)/$(1).pg $(HERE)/$(GEN)

$(1): $(1)-main.hs $(GEN)/$(2)Lexer.hs $(GEN)/$(2)Parser.hs $(GEN)/$(2)QQ.hs
	cd $(ROOT) && cabal exec -- ghc --make $(HERE)/$(1)-main.hs -i$(HERE)/$(GEN) -outputdir $(HERE)/build/$(1) -o $(HERE)/$(1)

test-$(1): $(1)
	./$(1)

repl-$(1): $(1)
	./$(1) repl
endef

$(eval $(call lang-rules,lc,Lc))
$(eval $(call lang-rules,stlc,Stlc))
$(eval $(call lang-rules,poly,Poly))
$(eval $(call lang-rules,proto,Proto))

$(GEN)/%.hs: $(GEN)/%.x
	cd $(ROOT) && cabal exec alex -- -g $(HERE)/$< -o $(HERE)/$@

$(GEN)/%.hs: $(GEN)/%.y
	cd $(ROOT) && cabal exec happy -- $(HERE)/$< --ghc -i$(HERE)/$(GEN)/$*.info -o $(HERE)/$@

clean:
	rm -rf $(GEN) build $(LANGS)
