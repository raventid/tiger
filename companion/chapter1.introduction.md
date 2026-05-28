# Introduction

At the beginning of Chapter 1 there is a paragraph that says:

> The interfaces between modules of the compiler are almost as important as the
> algorithms inside the modules. To describe the interfaces concretely, it is
> useful to write them down in a real programming language. This book uses ML —
> a strict, statically typed functional programming language with modular
> structure. ML is well suited to many applications, but compiler
> implementation in particular seems to hit all of its strong points...

A reminder: our companion guide uses OCaml, a language of the same family. Some
of the SML features presented in the book are not available to us in exactly
the same form, so we substitute them with a similar feature or a workaround
where possible.

Later the book mentions:

> This is not a textbook on ML programming. Students using this book who do not
> know ML already should be able to pick it up as they go along, using an ML
> programming book such as Paulson [1996] or Ullman [1994] as a reference.

In this companion we will elaborate on OCaml syntax and language as we go. If
you want to really learn enough OCaml to be confident implementing any kind of
program, please refer to a dedicated video course or a practical OCaml book as
a reference alongside this text.

## 1.2 Tools and software

> ML of New Jersey system, including associated tools such as its ML-Yacc,
> ML-Lex, and the Standard ML of New Jersey Software Library.

We are lucky as well: our language of choice, OCaml, has the fantastic Menhir
library, which lets us achieve the same result.

## 1.3 Data structures for tree languages

```ocaml
type id = string

type binop = Plus | Minus | Times | Div

type stm =
  | CompoundStm of stm * stm
  | AssignStm of id * exp
  | PrintStm of exp list

and exp =
  | IdExp of id
  | NumExp of int
  | OpExp of exp * binop * exp
  | EseqExp of stm * exp
```

*Program 1.5 — Representation of straight-line programs.*

As you can see, in OCaml we express the data structure in a way very similar to
SML.

### `open` declarations

OCaml has `open`, which works essentially the same way as SML's:

```ocaml
open A.F
open A.G
open B
open C
```

After this, you can write `X.put ()` without qualifying `X` with its containing
module — and just like in SML, the reader has to hunt through other files to
figure out where `X` came from. The same readability problem applies.

OCaml also offers a *local* open, which SML lacks and which can mitigate some of
the downsides:

```ocaml
let x = A.F.(put (); get ())          (* parenthesized form *)
let y = let open A.F in put (); get () (* let-open form *)
```

These limit the scope of the `open` to a single expression, making the source of
names much clearer.

### Structure (module) abbreviations

OCaml has module aliases, which are the direct equivalent of SML's structure
abbreviations:

```ocaml
module W = A.F.W
module X = A.G.X
module Y = B.Y
module Z = C.Z
```

These can appear at the top of a file (or anywhere a module definition is
allowed), and they give you exactly the property the author recommends: a reader
can see at a glance that `X` refers to `A.G.X` without leaving the file.

Note that unlike SML's `structure W = A and X = B` (which uses `and` to chain
them), OCaml requires a separate `module` keyword for each. You can use
`module rec ... and ...` only for *recursive* module definitions — plain aliases
are written as separate declarations.

### Straight-line program interpreter (exercise)

For the first exercise we provide this OCaml base version of the code. As this
companion guide uses OCaml instead of SML, we suggest you practise OCaml and
implement this program in it.

```ocaml
let prog =
  CompoundStm (
    AssignStm ("a", OpExp (NumExp 5, Plus, NumExp 3)),
    CompoundStm (
      AssignStm ("b",
        EseqExp (
          PrintStm [IdExp "a"; OpExp (IdExp "a", Minus, NumExp 1)],
          OpExp (NumExp 10, Times, IdExp "a"))),
      PrintStm [IdExp "b"]))
```

Now the book asks us to implement some code. Because this is a companion book,
the implementation here follows immediately after the exercise — so be careful
and try to solve the problem first, before looking at the solution.

1. Write a function `maxargs : stm -> int` that tells the maximum number of
   arguments of any `print` statement within any subexpression of a given
   statement. For example, `maxargs prog` is `2`.

2. Write a function `interp : stm -> unit` that "interprets" a program in this
   language. To write in a "functional" style — without assignment (`:=`) or
   arrays — maintain a list of `(variable, integer)` pairs and produce new
   versions of this list at each `AssignStm`.

> **TODO**
>
> - First solution
> - Second solution

## Chapter exercises

> **TODO** — to be written.
