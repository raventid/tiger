ch2_lexer:
	dune exec _build/default/chapter2/tiger/driver.exe $(FILE)

ch3_tig:
	dune exec _build/default/chapter3/tiger/driver.exe

ch4_tig:
	dune exec _build/default/chapter4/tiger/driver.exe

lex_all:
	bash ./scripts/lex_all_the_test_programs.sh

build:
	dune build
