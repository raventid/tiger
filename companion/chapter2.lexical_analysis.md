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

Book also shows you the representation of this automaton in table form which I extended to be full. If you want to feel, how transitioning are happening you can load this table and travel through it.

```ocaml
(* Combined finite automaton from Figure 2.4 -- full transition table.
   Rows are states 0..13.  A 0 entry means "no transition" (dead state).
   "ot" = any other character (anything not given its own column). *)

  let edges : int array array =
  [|
  (* col idx:        0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36  37  38  39  40 *)
  (* char:          sp  \n   .   -   0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f   g   h   i   j   k   l   m   n   o   p   q   r   s   t   u   v   w   x   y   z  ot *)
  (* state  0 *) [|  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state  1 *) [| 12; 12;  5;  9;  7;  7;  7;  7;  7;  7;  7;  7;  7;  7;  4;  4;  4;  4;  4;  4;  4;  4;  2;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4; 13 |];
  (* state  2 *) [|  0;  0;  0;  0;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  3;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  0 |];
  (* state  3 *) [|  0;  0;  0;  0;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  0 |];
  (* state  4 *) [|  0;  0;  0;  0;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  4;  0 |];
  (* state  5 *) [|  0;  0;  0;  0;  6;  6;  6;  6;  6;  6;  6;  6;  6;  6;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state  6 *) [|  0;  0;  0;  0;  6;  6;  6;  6;  6;  6;  6;  6;  6;  6;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state  7 *) [|  0;  0;  8;  0;  7;  7;  7;  7;  7;  7;  7;  7;  7;  7;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state  8 *) [|  0;  0;  0;  0;  8;  8;  8;  8;  8;  8;  8;  8;  8;  8;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state  9 *) [|  0;  0;  0; 10;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state 10 *) [|  0; 11;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10; 10;  0 |];
  (* state 11 *) [|  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state 12 *) [| 12; 12;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  (* state 13 *) [|  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0;  0 |];
  |]
```

You can play around this table by using Ocaml arrays. 

You start at the State 1, this is an initial state. State 0 or just 0 in one of the row/column means that we consumed the token and next symbol belong to something else.
The whole idea is to answer the question if given string belongs to our language. (Same question as for regular expression, because regular expression is just another form of the table above!)

To answer the question if string "julian" belongs to our language we can follow our table like this:
```ocaml
state = 1
'j' : edges.(1).(23) = 4    (* not 'i', so the generic-letter edge → 4 *)
'u' : edges.(4).(34) = 4    (* state 4 loops on every letter/digit *)
'l' : edges.(4).(25) = 4
'i' : edges.(4).(22) = 4
'a' : edges.(4).(14) = 4
'n' : edges.(4).(27) = 4
end : state 4  →  ACCEPT as IDENTIFIER
```
To fully implement lexer we need to have additional machinery, we can run lexer in loop and track the cursor. All we have to do in the loop is to repeatedly call a procedure that will jump through the table above.
Is it how real lexers work? It could be! But some other use slightly different mechanicks, but we will not focus on this too much here.


By looking at the table above you can follow FIGURE 2.5 from the book and see how full lexer pass would look like.

## 2.4 NONDETERMNISTIC FINITE AUTOMATA
Epsilon node allows us to make a nondeterminist choice or just a choice. If in the table above we have only one way to go from the one cell, now we can have 2!

There important quality of epsilon transition is that we can use it any time we want to express idea of transition to another state without consuming an input, let me bring an example.

Nondeterministic automata are a useful notion because it is easy to convert a (static, declarative) regular expression to a (simulatable, quasi-executable) NFA. (quoting the book)

tail - start edge
head - ending state


How to convert regex to NFA you can also watch in a very good tutorial by Easy Theory - https://www.youtube.com/watch?v=VbR1mGdP99s

computation of epsilon-closure means there are no states reachable without eating the first character of the input left.

### Converting an NFA to DFA

Book shows you algebraic and logical structure of NFA to DFA convertion, but I personally find it beneficial to see a visual transformation, so let's watch one more video - https://www.youtube.com/watch?v=SCdsgVT6Ktw

## 2.5 A lexical analyzer generator
Book will talk about ml-lex a SML library to produce a lexer from a lexical specification.

As in this guide we are using Ocaml instead of the SML I will give you a brief introduction to ocamllex - the library we can use to write lexer for our language.

## 2.2 Running the lexer

You can run the Chapter 2 lexer over a sample Tiger program — for example the
eight-queens solution — with:

```sh
make ch2_lexer FILE=shared/sources/queens.tig
```

> **TODO** — walk through the `ocamllex` specification and explain how regular
> expressions map to the token rules above.

## Chapter exercises

I have an implementation of a lexer for sure, but just copy pasting it is not very fun, try to come up with your own and use LLM to study more about ocamllex and how it should be used.

To make sure a knowledge will stick to your memory, let's do some exercises without an LLM.




For each of the following, explain why you're not surprised that there is no
regular expression defining it.
a. Strings of a's and b's where there are more a's than b's.
b. Strings of a's and b's that are palindromes (the same forward as backward).
c. Syntactically correct ML programs.

All these group of questions talking about sequence of characters maintaining some invariant. We can express patterns in regular expressions, but we cannot analyze semantics (c.) or build regexp that support logical connections between elements (a, b)
