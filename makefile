lexer = Lexer.hs
parser = Parser.hs

default: main

$(lexer): Lexer.x
	alex -o $@ $<

$(parser): Parser.y
	happy -o $@ $<

main: $(lexer) $(parser)
	ghc --make main

clean:
	rm -f $(lexer) $(parser) main

.phony: clean build default
