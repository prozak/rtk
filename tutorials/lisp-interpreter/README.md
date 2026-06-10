# lis.py on RTK: a Lisp interpreter from a grammar file

Peter Norvig's essay [*(How to Write a (Lisp) Interpreter (in
Python))*](https://norvig.com/lispy.html) builds a working Scheme
interpreter, `lis.py`, in about 90 lines of Python. Roughly a third of
those lines — and most of the fragility — are the *reader*: a tokenizer,
a recursive token consumer, and an atom classifier.

This tutorial reimplements `lis.py` with RTK. The reader is replaced by a
15-line grammar, and the two places where an interpreter pattern-matches on
program shapes — special-form dispatch in `eval` and macro expansion — are
written as quasi-quotation patterns that RTK checks with the real parser at
compile time.

## Layout

| File | Role |
|------|------|
| [`scheme.pg`](scheme.pg) | The grammar — replaces lis.py's `tokenize`, `read_from_tokens` and `atom`. Everything under `gen/` is generated from it. |
| [`Main.hs`](Main.hs) | The interpreter: `eval`, `expand` (the macro expander), `Env`, the standard environment, `schemestr`, the REPL, and the test suite. |
| [`examples/`](examples/) | Sample programs (`fact.scm`, `fib.scm`). |

## Building and testing

The tutorial borrows all Haskell tooling from the RTK checkout two levels
up (`cabal exec rtk/alex/happy/ghc`), so build RTK first:

```bash
cd ../..
cabal build          # plus the toolchain setup described in /CLAUDE.md
cd tutorials/lisp-interpreter

make build           # scheme.pg -> gen/{SchemeLexer,SchemeParser,SchemeQQ} -> lis
make test            # Norvig's test cases (./lis --test) + the examples
make repl            # the REPL
```

## 1. The language

The same Scheme subset as `lis.py`: numbers, symbols, `(quote ...)`,
`(if test conseq alt)`, `(define var exp)`, `(set! var exp)`,
`(lambda (params...) body)`, procedure calls, and a standard environment
with arithmetic and list primitives. On top of that, this port adds
strings, booleans (`#t`/`#f`), comments — and a macro expander for
`let`, `when`, `unless`, `and`, `or`.

## 2. The reader: 30 lines of Python vs. a grammar file

`lis.py` reads programs by inserting spaces around parentheses, splitting
on whitespace, and consuming the token list recursively:

```python
def tokenize(chars: str) -> list:
    return chars.replace('(', ' ( ').replace(')', ' ) ').split()

def read_from_tokens(tokens: list) -> Exp:
    ...
def atom(token: str) -> Atom:
    try: return int(token)
    except ValueError:
        try: return float(token)
        except ValueError:
            return Symbol(token)
```

It works, but it is the part everyone trips over: it cannot handle
strings (`"a (b"` splits in the middle), there are no comments, no line
numbers in errors, and extending it means rewriting it.

The RTK port describes the language instead ([`scheme.pg`](scheme.pg)):

```
grammar 'Scheme';

@shortcuts(x, c, t, e, a, b, v, p, f, q)
Expr = num
     | flt
     | str
     | '#t'
     | '#f'
     | sym
     | '(' Expr* ')' ;

str = [\"] ([^\\x22\\x5C\\x0A\\x0D] | [\\x5C] .)* [\"] ;
sym = ([a-zA-Z_*/>=!] | '+' | '-' | '<' | '?')
      ([a-zA-Z0-9_*/>=!] | '+' | '-' | '<' | '?')* ;
Double:  flt = ('-')? [0-9]+ '.' [0-9]+ ;
Integer: num = ('-')? [0-9]+ ;
Ignore: ws = [ \t\n\r]+ ;
Ignore: comment = ';' .* ;
```

Everything is an `Expr`: an atom or a parenthesized list of expressions.
Note what is *not* here: `if`, `define` and `lambda` are not keywords.
They are ordinary symbols, and special forms are ordinary lists — exactly
the lis.py architecture, where `eval` decides what a list means.

Points worth noticing:

- **Typed tokens.** `Integer: num = ...` makes the lexer hand the parser
  an `Integer`, not a string (`read` is applied for you); likewise
  `Double:`. Bignums work out of the box: `(fact 50)` below produces all
  65 digits, where lis.py relies on Python's ints.
- **Strings and comments cost one line each** — the two things the
  whitespace-splitting tokenizer fundamentally cannot do.
- **Longest match resolves `-`**: `-5` is one number token, `(- 5 1)`
  lexes `-` as a symbol. lis.py gets the same effect from `int(token)`
  failing on `-`. (On a length tie, the rule declared later in the `.pg`
  wins; `num` is declared after `sym` for exactly this reason.)
- **`sym` mixes a character class with string literals**: `+`, `-`, `<`
  and `?` are operators inside Alex character sets, so the rule matches
  them as quoted alternates instead of class members.
- `@shortcuts` is explained in §5.

From this file `rtk` generates `SchemeLexer.x` (Alex), `SchemeParser.y`
(Happy) and `SchemeQQ.hs` (the quasi-quoter), and the AST type comes out
as:

```haskell
data Expr = ...
          | Ctr__Expr__1 RtkPos Integer   -- num
          | Ctr__Expr__2 RtkPos Double    -- flt
          | Ctr__Expr__3 RtkPos String    -- str
          | Ctr__Expr__4 RtkPos           -- #t
          | Ctr__Expr__5 RtkPos           -- #f
          | Ctr__Expr__6 RtkPos String    -- sym
          | Ctr__Expr__7 RtkPos [Expr]    -- ( ... )
```

Every constructor records where it was parsed in a leading `RtkPos`
field (project it from any node with `rtkPosOf`). Positions are
transparent for equality, and quasi-quote patterns wildcard them, so they
never get in the way of matching.

RTK names constructors positionally. For the few places that match on
them directly, the port defines pattern synonyms once — matching ignores
the position, construction uses the `rtkNoPos` placeholder:

```haskell
pattern Sym s <- Ctr__Expr__6 _ s
  where Sym s = Ctr__Expr__6 rtkNoPos s
pattern List xs <- Ctr__Expr__7 _ xs
  where List xs = Ctr__Expr__7 rtkNoPos xs
-- ... and Num, Flt, Str, TrueL, FalseL
```

Most of the interpreter never touches constructor names at all — it
matches *Scheme syntax*, as the next sections show.

## 3. Programs, values, environments

`lis.py` blurs programs and values: a parsed program *is* nested Python
lists, and `eval` returns those same lists. In a typed language the two
are separate types, and the boundary becomes visible — and instructive:

```haskell
data Value = VNum Integer | VFlt Double | VStr String | VBool Bool
           | VSym String  | VList [Value]
           | VClosure [String] Expr Env
           | VPrim String ([Value] -> IO Value)
           | VUnspecified
```

`(quote exp)` is precisely the function that reflects program text into a
value: `datumValue :: Expr -> Value` walks the AST and rebuilds it as
`VList`/`VSym`/... — eight lines, and code-as-data still works:

```
lis.hs> (quote (if 1 2 3))
(if 1 2 3)
```

Environments are Norvig's `Env` class, IORef'd maps with an outer
pointer; `findFrame` is his `env.find`, returning the innermost frame
that binds a name, which is what makes `set!` and closures behave:

```haskell
data Env = Env (IORef (M.Map String Value)) (Maybe Env)
```

## 4. eval: the elif chain becomes syntax

The heart of `lis.py`:

```python
def eval(x, env=global_env):
    if isinstance(x, Symbol):
        return env.find(x)[x]
    elif not isinstance(x, List):
        return x
    elif x[0] == 'quote':
        (_, exp) = x
        return exp
    elif x[0] == 'if':
        (_, test, conseq, alt) = x
        exp = (conseq if eval(test, env) else alt)
        return eval(exp, env)
    elif x[0] == 'define':
        ...
```

The shapes being matched — `(quote exp)`, `(if test conseq alt)` — are
written as Python tuple unpacking against `x`, with the connection to
Scheme syntax held in the reader's head. In the RTK port the shapes are
written *as the syntax itself*, in quasi-quotation patterns:

```haskell
eval :: Env -> Expr -> IO Value

eval _   [expr| (quote $x) |] = pure (datumValue x)

eval env [expr| (if $c $t $e) |] = do
    v <- eval env c
    eval env (if truthy v then t else e)

eval env [expr| (define $v $e) |] = case v of
    Sym name -> do val <- eval env e
                   defineVar env name val
                   pure VUnspecified
    other -> schemeError ("define: expected a symbol, got " ++ showExpr other)

eval env [expr| (lambda $p $b) |] = pure (VClosure (paramNames p) b env)

eval env (Sym name)        = lookupVar env name
eval env (List (f : args)) = do fv <- eval env f
                                vs <- mapM (eval env) args
                                apply fv vs
eval _   atom              = pure (datumValue atom)
```

What is happening in `[expr| (if $c $t $e) |]`:

- At **compile time**, RTK parses the text `(if $c $t $e)` with the same
  generated parser that parses programs. A typo — `(fi $c $t $e)`, a
  missing `)` — is a compile error in `eval`, not a latent bug.
- `$c`, `$t`, `$e` are **antiquotations**: holes that become bound
  Haskell variables of type `Expr`. The pattern compiles to "a 4-element
  list whose head is the symbol `if`" — the same thing Norvig's
  `(_, test, conseq, alt) = x` means, but checked.
- The same quoter works in **expression position** to build syntax, as
  §6 shows.

Because `if` is just a symbol in the grammar, clause order plays the role
of the elif chain: special forms first, then variables, then the generic
application clause. That last clause uses an ordinary Haskell list
pattern, `List (f : args)` — QQ patterns describe *fixed* shapes, so
"head plus however many arguments" is where the pattern synonyms earn
their keep (see §8).

## 5. Antiquotation names and `@shortcuts`

How does `$c` know it is an `Expr`? RTK resolves an antiquote variable's
type from its name's prefix, using the table declared in the grammar:

```
@shortcuts(x, c, t, e, a, b, v, p, f, q)
Expr = ...
```

Each shortcut maps a prefix to the rule's type, so `$c`, `$cond`,
`$body` (prefix `b`) all parse as `Expr` holes. The lowercased rule name
itself is always available (`$expr1`), and the explicit form `$Expr:name`
works for any name with no declared prefix. With a single-rule grammar
this is trivia; in a multi-rule grammar (see the parent repo's
[`test-grammars/p.pg`](../../test-grammars/p.pg)) the prefixes pick which
nonterminal each hole stands for.

## 6. Macro expansion: quasi-quotation on both sides

`lis.py` stops at special forms; its big sibling `lispy.py` adds derived
forms by hand-rewriting Python lists. This is where RTK pays off most.
The port runs every form through `expand` before `eval`, and the rewrite
rules are QQ patterns on the left and QQ templates on the right:

```haskell
expand :: Expr -> Expr
expand q@[expr| (quote $x) |] = q                       -- don't touch data
expand [expr| (when $c $b) |]   = expand [expr| (if $c $b #f) |]
expand [expr| (unless $c $b) |] = expand [expr| (if $c #f $b) |]
expand [expr| (and $a $b) |]    = expand [expr| (if $a $b #f) |]
expand [expr| (or $a $b) |]     = expand [expr| ((lambda (or-tmp) (if or-tmp or-tmp $b)) $a) |]
expand [expr| (let (($v $e)) $b) |] = expand [expr| ((lambda ($v) $b) $e) |]
expand (List (Sym "let" : List bindings : body))        -- general let
  | Just pairs <- mapM bindingPair bindings =
      let (vars, exps) = unzip pairs
          lam = List [Sym "lambda", List (map Sym vars), beginWrap body]
      in expand (List (lam : exps))
expand (List xs) = List (map expand xs)
expand e = e
```

Read the `let` line again — it is the textbook rewrite, written as
itself: `(let ((v e)) b)` ⇒ `((lambda (v) b) e)`. Both sides go through
the Scheme parser when *this file* compiles; the holes are the only
variables. Compare against doing it with raw constructors:

```haskell
-- the same rule without quasi-quotation:
expand (List [Sym "let", List [List [v, e]], b]) =
    expand (List [List [Sym "lambda", List [v], b], e])
```

Both work; one of them can be checked against the Scheme report by eye.

Two honest footnotes, both of which make good exercises:

- **Hygiene.** `or` must return the first operand's value, so it binds a
  temporary — and a user variable named `or-tmp` would be captured. Real
  Scheme macro systems generate fresh names; `expand` could too (thread a
  counter), at the cost of no longer being a pure one-liner.
- **Fixed shapes.** A QQ pattern matches an exact arity, so the
  single-binding `let` is one elegant line while *n*-binding `let` drops
  to list manipulation (still six lines, and the pattern synonyms keep it
  readable). Segment antiquotes (`$es...` matching "the rest") are the
  RTK feature this tutorial most obviously motivates.

## 7. The REPL and running files

`schemestr` and `repl` port directly (with `lis.hs>` standing in for
`lis.py>`). One trick replaces a file loader: the generated parser parses
one expression, and a file is a sequence of forms — so `runFile` wraps
the file text in `(begin ... )` and evaluates that. `begin` is a
primitive that returns its last argument, and arguments evaluate left to
right, so top-level `define`s run in order — the same reason
`(begin (define r 10) (* pi (* r r)))` works in lis.py.

```
$ ./lis
lis.hs -- Norvig's lis.py on RTK (Ctrl-D to exit)
lis.hs> (define circle-area (lambda (r) (* pi (* r r))))
lis.hs> (circle-area (+ 5 5))
314.1592653589793
lis.hs> (+ 1
parse error: line 1, column 5: unexpected end of input
lis.hs> (undefined-thing 1)
scheme error: unbound symbol: undefined-thing
```

Parse errors come with positions, and a bad line doesn't kill the
session: the generated lexer and parser return `Either`, with positions
encoded in the error string (and carried on every AST node — see
`RtkPos` in §2).

## 8. Did it survive the port? Norvig's own tests

`./lis --test` runs the `lis.py`/`lispytest.py` cases (adapted
where Python truthiness leaks into the originals) plus the derived-form
tests. The full suite — 47 cases — passes; highlights:

```
ok   ((lambda (x) (+ x x)) 5)  => 10
ok   ((repeat (repeat twice)) 5)  => 80
ok   (fact 50)  => 30414093201713378043612608166064768844377641568960512000000000000
ok   (define my-abs (lambda (n) ((if (> n 0) + -) 0 n)))
ok   (list (my-abs -3) (my-abs 0) (my-abs 3))  => (3 0 3)
ok   (map fib (range 0 10))  => (1 1 2 3 5 8 13 21 34 55)
ok   (let ((x 2) (y 3)) (* x y))  => 6
ok   (or (> 1 2) 7)  => 7
```

Deliberate divergences from `lis.py`, all of them arguments rather than
accidents:

| | lis.py | this port |
|---|---|---|
| truth | Python truthiness (`0`, `()` are false) | only `#f` is false |
| `eq?` | object identity (`op.is_`) | same as `equal?` (values are immutable) |
| `/` on integers | float division | integer division |
| arity errors | silent `zip` truncation | reported (`expected 2 argument(s), got 3`) |
| malformed `(define 5 x)` | unpacking crash | `define: expected a symbol, got 5` |

## 9. Where to go from here

- **Push special forms into the grammar.** The parent repo's
  [`test-grammars/p.pg`](../../test-grammars/p.pg) shows the other
  design: `E = '(' 'if0' E E E ')' | ...` gives each form its own
  constructor and makes `(if)` a *parse* error instead of a runtime one.
  The price is that `quote` needs explicit reflection and `if` stops
  being a usable variable name. Porting this interpreter to that style is
  a one-evening exercise.
- **Quote sugar.** `'x` for `(quote x)` is one grammar alternative —
  compare with what it costs in a hand-rolled tokenizer.
- **More of lispy.py**: tail calls (make `eval` loop instead of recurse),
  `define-macro` (let users add `expand` rules at runtime), proper
  hygiene for the expander.

The whole port is: a 15-line grammar, about 560 lines of ordinary,
commented Haskell (a third of it the standard environment, another chunk
the test suite), and zero hand-written parsing code.
