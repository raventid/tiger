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

Tokens like `class` are called reserved words. There are also non-tokens:
```
comment /* try again */
preprocessor directive #include<stdio. h >
preprocessor directive # define NUM S 5 , 6
macro NUMS
blanks, tabs, and newlines
```

This chapter also goes deep into mathematical connection of regular expressions and deterministic finite automata.
I will touch this briefly, but if it does not click for you, you can try to watch youtube video with a more visual
explanation of the same topic **TODO:**

What you should capture from this chapter is understanding that core regular expression lanugage is very small, there are a lot of extensions, but the core
itself is tiny.

In modern compiler design lexing and parsing is considered to be a solved problem. We know enough datastructures and algorithms to implement optimal solutions for any lexing or parsing tasks. Because of this I do not recommend you (unless you want) to focus and get stuck on early chapters. I personally see lexing and parsing as a boring machinery to get to the things we care about (optimizations, code generation) later.

(**TODO: elaborate on picture 2.4 and give explanation to the matrix form?**)
Picture 2.4 in the book shows how to connect small regular expressions schemas into one big machine, we will need this to write code, so it is an important concept!

full TABLE GOES HERE.

I extended this table to cover the whole automaton on the picture 2.4. I hope now you can see that this is just one of possible encodings that explicitly connects every transition with every node. Think how can you encode this in programming language? Maybe create a big state machine and give it one token after another one?

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
