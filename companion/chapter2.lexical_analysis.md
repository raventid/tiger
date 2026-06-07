# Lexical Analysis

The first real task of a compiler is to break the source file into a stream of
*tokens*: keywords, identifiers, literals, punctuation, and so on. This stage is
called *lexical analysis* (or *lexing*), and it is the subject of Chapter 2 in
the original book.

As Appel writes:
"...It would unduly complicate the parser to have to
account for possible white space and comments at every possible point; this
is the main reason for separating lexical analysis from parsing...". Take notes.

In the companion we build the lexer for Tiger in OCaml. Where the original text
uses ML-Lex, we use OCaml's `ocamllex` together with `Menhir` for the parser in
later chapters.

I will also show you how to separate `ocamllex` and `menhir` ouputs, and why you might want to do this. And not many people know how to do it, so you can flex!

## 2.1 Lexical tokens

A token is a sequence of characters treated as a unit by the grammar of the
language. So, this is like an atomic object, you cannot divide it and make it smaller. The Tiger lexer recognises tokens such as:

```text
ID      foo bar baz
INT     42 0 1000
STRING  "hello"
keywords  if then else while for to do let in end
symbols   ( ) [ ] { } : := . , ; + - * / = <> < <= > >=
```

## 2.2 Running the lexer

You can run the Chapter 2 lexer over a sample Tiger program — for example the
eight-queens solution — with:

```sh
make ch2_lexer FILE=shared/sources/queens.tig
```

> **TODO** — walk through the `ocamllex` specification and explain how regular
> expressions map to the token rules above.

## Chapter exercises

> **TODO** — to be written.
