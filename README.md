# tiger

Just another implementation of tiger programming language in Ocaml

Repo is structured to implement a langauge step by step.


Download tiger program samples:

``` sh
wget -r -np -nc -A "*.tig" https://www.cs.princeton.edu/~appel/modern/testcases/
```

## Chapter 2

Calling a lexer with an 8-queen solution

``` sh
make ch2_lexer FILE=shared/sources/queens.tig
```

## Chapter 3

To preprocess Tiger source code files I use a bash one linear which uses lexer implentation from Chapter2
