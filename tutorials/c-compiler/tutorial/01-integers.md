# 01 — Integers

← [00 — Setup](00-setup.md) · [Tutorial index](README.md)

Companion to **[Writing a C Compiler, Part 1](https://norasandler.com/2017/11/29/Write-a-Compiler.html)**.
Read that first; this page shows what changes when RTK generates the front
end.

The goal is the same as the blog's: compile the smallest interesting C
program,

```c
int main() {
    return 2;
}
```

into an executable that exits with code 2. Part 1 builds a lexer, an AST and a
parser, a code generator, and a driver. We will take them in the same order
and see what RTK does to each.

## 1. One grammar replaces the lexer, the AST, and the parser

> **Blog ⇄ RTK.** Part 1 hand-writes three things: a list of token types plus
> a scanner; a set of AST data types; and a recursive-descent parser that
> turns tokens into the AST. In this tutorial all three come out of one file,
> [`c.pg`](../c.pg).

Part 1 gives the stage-1 grammar in BNF:

```
<program>   ::= <function>
<function>  ::= "int" <id> "(" ")" "{" <statement> "}"
<statement> ::= "return" <exp> ";"
<exp>       ::= <int>
```

`c.pg` is that grammar, written in RTK's syntax:

```
Program = Prog: Function ;

Function = Func: 'int' Ident '(' ')' '{' StatementList '}' ;

@shortcuts(stmts)
StatementList = Statement* ;

Statement = Return: 'return' Exp ';' ;

@shortcuts(e)
Exp = IntLit: intLit ;

@shortcuts(name)
Ident = Name: id ;

# Lexical rules
id = [a-zA-Z_][a-zA-Z_0-9]* ;
Int: intLit = [0-9]+ ;
Ignore: ws = [ \t\n\r]+ ;
Ignore: comment = '//' [^\n]* '\n' ;
Ignore: blockComment = '/*' ([^*\n] | [\n]) ([\n] | [^*\n] | [*] [^/\n] | [*] [\n])* '*/' ;
```

A rule whose name starts with an **uppercase** letter is a *syntax* rule (it
becomes a nonterminal and an AST type); a **lowercase** rule is a *lexical*
rule (it becomes a token). So the block above the comment is the parser
grammar, and the block below it is the lexer — one notation for both.

A few things to notice, each of which the blog does differently:

- **The function body is a statement list** (`StatementList = Statement*`),
  not the single `<statement>` of the part-1 BNF. The blog switches to a list
  in part 5; starting there costs nothing now and lets stage 1 exercise list
  antiquotation. `*` means zero-or-more and produces a Haskell list.
- **`Int: intLit = [0-9]+`** says the `intLit` token carries an `Int` payload
  (RTK will `read` the matched digits), rather than the raw string.
- **`Ignore:`** rules are lexed and discarded — whitespace and both comment
  forms — so the parser never sees them. This is the blog's "skip whitespace"
  loop, declared instead of coded.
- **The leading labels** — `Prog:`, `Func:`, `Return:`, `IntLit:`, `Name:` —
  name the AST constructor RTK generates for each alternative. Without them
  the names are positional (`Ctr__Exp__0`), encoding the alternative's index,
  so reordering the grammar silently renames them; the labels make them
  stable and readable. (See convention 6 in the [project README](../README.md).)

### What RTK generates from it

The **AST types** Sandler writes by hand, RTK derives from the syntax rules
(in `gen/CParser.hs`, shown here one type per line):

```haskell
data Exp       = Anti_Exp String       | IntLit RtkPos Int
data Statement = Anti_Statement String | Return RtkPos Exp
data Ident     = Anti_Ident String     | Name   RtkPos String
data Function  = Anti_Function String  | Func   RtkPos Ident StatementList
type StatementList = [Statement]
```

These are exactly the shapes the blog defines — `Return` wraps an `Exp`,
`IntLit` holds an `Int` — with two RTK additions:

- The first field of every real constructor is an `RtkPos`, the node's source
  position. AST equality ignores it (two nodes that differ only in position
  compare equal), which matters later for matching quasi-quoted trees against
  parsed ones.
- Every type has an `Anti_X String` constructor. That is the hook for
  antiquotation — `$e` in a quasi-quote becomes an `Anti_Exp "e"` node. You
  never write these by hand; section 2 shows them doing their job.

The start type, `Program`, additionally carries a handful of
`Ctr__Program__N` constructors alongside `Prog`; those are quasi-quoter
scaffolding (they let `[program| … |]` stand in for any sort) and no code
ever names them.

The **lexer** Sandler writes as a token list plus a scanner; RTK emits an
Alex specification (`gen/CLexer.x`) whose token rules are the lexical part of
the grammar:

```
"int"     { simple Tk__tok_int_0 }
"return"  { simple Tk__tok_return_5 }
"("       { simple Tk__tok__lparen__1 }
...
([0-9]+)               { simple1 $  Tk__intLit . (read) }
([a-zA-Z_]  [a-zA-Z_0-9]*) { simple1 $  Tk__id . (id) }
([\ \t\n\r]+) ;
```

Each keyword and symbol becomes a token; `intLit` and `id` carry payloads
(`read` turns the matched digits into an `Int`); and a rule with a bare `;`
action and no token — like the `Ignore: ws` whitespace rule above — is
matched and thrown away.

And the **parser** is the generated Happy grammar in `gen/CParser.hs`,
reached through `parseC`. You wrote a grammar; you got a lexer, an AST, and a
parser.

## 2. Code generation with quasi-quotation

> **Blog ⇄ RTK.** Part 1's code generator is a function that recurses over the
> AST and `print`s assembly strings. Here, [`Codegen.hs`](../Codegen.hs)
> *pattern-matches* the AST with quasi-quotes and *builds* an assembly AST
> with quasi-quotes — no string handling at all.

The stage-1 code generator, minus its `other = error …` fallthrough arms:

```haskell
codegen :: Program -> Asm
codegen [program| int $name ( ) { $stmts } |] =
  let sym = mkSym (identName name)
      items =
        concatMap genStatement stmts
          -- C99 5.1.2.2.3: falling off the end of main returns 0
          ++ [asmItems|
               movl $0, %eax
               ret
             |]
  in [asm|
       .globl $sym
       $sym :
       $items
     |]

genStatement :: Statement -> [AsmItem]
genStatement [statement| return $e ; |] =
  let src = mkImm (expValue e)
  in [asmItems|
       movl $src, %eax
       ret
     |]
```

Read the patterns as concrete syntax with holes. `[program| int $name ( ) {
$stmts } |]` matches a whole function and binds `name` to its `Ident` and
`stmts` to its `[Statement]`. `[statement| return $e ; |]` matches a return
and binds `e` to the returned `Exp`. These are the generated quasi-quoters
from `gen/CQQ.hs`, named after the grammar's types (`program`, `statement`);
`$name`, `$stmts`, `$e` resolve through the `@shortcuts` you declared in
`c.pg`.

The right-hand sides build assembly the same way. `[asmItems| movl $src, %eax
… |]` is not a string — it is a quasi-quote in the **assembly** grammar that
constructs `[AsmItem]` nodes, splicing in `src` (an `Operand`) and `$stmts`
(a list, spliced with the rest of the items around it). This is the second
grammar, [`asm.pg`](../asm.pg):

```
Asm = AsmUnit: AsmItems ;

@shortcuts(items)
AsmItems = AsmItem* ;

AsmItem = Globl: '.globl' AsmId
        | Label: AsmId ':'
        | Movl: 'movl' Operand ',' Operand
        | Ret: 'ret' ;

@shortcuts(src, dst)
Operand = Imm: '$' num
        | RegOp: Reg ;

Reg = Eax: '%eax' ;

@shortcuts(sym)
AsmId = Sym: asmid ;
```

So code generation is a translation from one RTK grammar's AST to another's,
and both ends are quasi-quoted. Sandler's `"\tmovl\t$" ++ show n ++ ",
%eax\n"` becomes `[asmItems| movl $src, %eax |]`.

> **The one rough edge: token payloads.** A `$`-antiquote splices or binds a
> whole *syntax sort*, never a raw token value. You cannot write the `Int`
> inside an `Exp`, or the `String` inside an `Ident`, as an antiquote. Those
> leaves go through the named constructors directly:
>
> ```haskell
> expValue (IntLit _ n) = n      -- read the Int out of an Exp
> identName (Name _ s)  = s      -- read the String out of an Ident
> mkImm = Imm rtkNoPos           -- build an immediate Operand from an Int
> mkSym = Sym rtkNoPos           -- build a symbol AsmId from a String
> ```
>
> `rtkNoPos` is the "no source position" placeholder for nodes built in code.
> This is exactly why naming the constructors (the `Imm:`/`IntLit:` labels)
> was worth doing — the alternative is matching `Ctr__Operand__0`.

A note on the assembly itself: `codegen` always appends a trailing `movl $0,
%eax; ret`, the C99 rule that running off the end of `main` returns 0. For
`return 2;` the first `ret` already fires, so the trailing pair is dead — but
it is correct, and it is why the output in section 5 has two `ret`s.

## 3. Turning the assembly AST into text

> **Blog ⇄ RTK.** The blog never has an assembly AST, so it never has this
> step — it printed strings in step 2. We have a tree, so something must
> render it. RTK generates *parsers*, not pretty-printers, so this half is
> hand-written — but it is small, and the generated assembly parser keeps it
> honest.

[`Emit.hs`](../Emit.hs) walks the assembly AST with the same quasi-quote
patterns and produces AT&T text:

```haskell
emit :: Asm -> String
emit [asm| $items |] = unlines (map emitItem items)

emitItem [asmItem| .globl $sym |]     = "    .globl " ++ symName sym
emitItem [asmItem| $sym : |]          = symName sym ++ ":"
emitItem [asmItem| movl $src, $dst |] = "    movl    " ++ emitOperand src ++ ", " ++ emitOperand dst
emitItem [asmItem| ret |]             = "    ret"
```

Because `asm.pg` also yields a *parser* (`parseAsm`), the emitter has a free
correctness check: parsing its output should reproduce the AST it started
from. `TestQQ.hs` asserts exactly that — `parseAsm (emit a) == a` — so a
formatting bug that produced unparseable or wrong assembly would fail a test
rather than ship.

> **Why not the generated pretty-printer?** RTK *does* generate one
> (`--generate-pp` produces an `AsmPP` module with `ppAsm :: Asm -> String`),
> so it is fair to ask why `Emit.hs` is hand-written. The generated `ppAsm`
> guarantees the round trip `parseAsm (ppAsm a) == a` — but against *RTK's
> own* parser, which treats newlines as insignificant whitespace. That is what
> lets the assembly grammar round-trip loosely; it is also why the printer
> puts the whole program on **one line**:
>
> ```
> .globl main main : movl $ 2 , %eax ret movl $ 0 , %eax ret
> ```
>
> The consumer here is **gas**, which is line-oriented, and it rejects that
> with "junk at end of line". Tellingly, gas tolerates the rest of the
> printer's spacing — `main :` and `$ 2` both assemble fine *once the
> statements are on separate lines* — so the only fatal gap is the missing
> line breaks between list elements. RTK's `block` layout inserts breaks for
> bracket-enclosed lists (a `{ … }` statement list), but assembly's
> instructions are a top-level `AsmItem*` with no enclosing brackets, so they
> stay on one line. A small PP option could close that gap (task R7 in the
> [plan](../../../docs/c-compiler-tutorial-plan.md)); until it exists,
> `Emit.hs` is the ~30 lines that produce gas-valid text — and, as a bonus,
> idiomatic formatting (indentation, `main:`, `$2`) the structural printer
> would not give.

> **Antiquote spacing.** The label pattern is written `[asmItem| $sym : |]`
> with a space before the `:`. Written `$sym:`, the `:` would be read as part
> of the explicit `$Rule:name` antiquote form and would not scan. Emitted
> labels still use the conventional `main:` — the space is only in the quote.

## 4. The driver

> **Blog ⇄ RTK.** Same job as the blog's driver — write the `.s`, call `gcc`,
> clean up — with RTK's `Either`-based error reporting and one Linux detail.

[`Main.hs`](../Main.hs):

```haskell
compileFile path = do
  src <- readFile path
  ast <- case scanTokens src >>= parseC of
    Left err  -> do hPutStrLn stderr (path ++ ": " ++ err); exitFailure
    Right ast -> return (ast :: Program)
  let asmPath = replaceExtension path "s"
      exePath = dropExtension path
  writeFile asmPath (emit (codegen ast) ++ gnuStackNote)
  rc <- rawSystem "gcc" [asmPath, "-o", exePath]
  removeFile asmPath
  when (rc /= ExitSuccess) exitFailure
```

The generated lexer and parser return `Either String`, so a malformed
program is a `Left` that is reported and rejected **before** any file is
written — that is the "no artifacts on failure" half of the driver contract.
`gnuStackNote` is a one-line ELF directive (`.section .note.GNU-stack,"",
@progbits`) appended when writing the `.s`; without it the linker warns that
the program implies an executable stack. It lives in the driver, not in
`codegen` or `emit`, because it is file-level packaging identical for every
program — and putting it in `emit` would break the round-trip property from
section 3.

## 5. Build it and run it

```bash
make build
printf 'int main() {\n    return 2;\n}\n' > return_2.c
./ncc return_2.c
./return_2; echo "exit code: $?"
```

```
exit code: 2
```

The assembly `codegen`/`emit` produce for this program (the driver appends
the GNU-stack directive when it writes the file):

```asm
    .globl main
main:
    movl    $2, %eax
    ret
    movl    $0, %eax
    ret
```

That is the same program Part 1 emits — `movl $2, %eax; ret` under a
`.globl main` / `main:` preamble — produced by building and rendering a
syntax tree instead of formatting strings, with the C99 fall-through pair
trailing.

## 6. Test it

The quasi-quotation feature matrix and the compiler tests:

```bash
make test
```

```
PASS  round trip: parse (emit asm) == asm
PASS  full pipeline: parse C -> codegen -> emit -> parse Asm
All quasi-quotation tests passed.
...
PASS  tests/valid/return_2.c (exit code 2)
...
PASS  tests/invalid/no_semicolon.c (rejected)
All stage-1 compiler tests passed.
```

And the acceptance test, Sandler's own suite:

```bash
/tmp/wacc/test_compiler.sh "$PWD/ncc" 1
```

```
===================Stage 1 Summary=================
12 successes, 0 failures
```

## What you wrote vs. what RTK generated

| Part 1 writes by hand | This tutorial |
|---|---|
| Token types + scanner | lexical rules in `c.pg` → generated `CLexer` |
| AST data types | generated from `c.pg`'s syntax rules |
| Recursive-descent parser | generated `CParser` (`parseC`) |
| Codegen recursing over the AST, printing strings | `Codegen.hs`: QQ patterns in, `asm.pg` AST built with splices out |
| (assembly as ad-hoc strings) | `asm.pg` grammar + `Emit.hs`, round-trip-tested |
| Driver calling `gcc` | `Main.hs` |

You wrote two grammars and three short Haskell modules; RTK generated the
lexer, the parser, the AST, and the quasi-quoters for both languages.

## Next

Stage 2 adds the unary operators `-`, `~`, and `!`, where `codegen` first
becomes recursive over nested expressions. It is written up — with the `c.pg`
and `asm.pg` growth and the RTK-specific gotchas — as task **C2** in
[`docs/c-compiler-tutorial-plan.md`](../../../docs/c-compiler-tutorial-plan.md),
and a page will appear here when it lands.
