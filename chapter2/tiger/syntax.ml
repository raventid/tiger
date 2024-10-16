type token =
    | TYPE
    | VAR
    | FUNCTION
    | BREAK
    | OF
    | END
    | IN
    | NIL
    | LET
    | DO
    | TO
    | FOR
    | WHILE
    | ELSE
    | THEN
    | IF
    | ARRAY
    | ASSIGN
    | OR
    | AND
    | GE
    | GT
    | LE
    | LT
    | NEQ
    | EQ
    | DIVIDE
    | TIMES
    | MINUS
    | PLUS
    | DOT
    | RBRACE
    | LBRACE
    | RBRACK
    | LBRACK
    | RPAREN
    | LPAREN
    | SEMICOLON
    | COLON
    | COMMA
    | STRING of string
    | INT of int
    | ID of string
    | EOF

let escape_char = function
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\\' -> "\\\\"
  | '\"' -> "\\\""
  | character -> String.make 1 character

let escape_string string =
  String.to_seq string
  |> Seq.map escape_char
  |> List.of_seq
  |> String.concat ""

let show = function
    | TYPE             ->     "TYPE"
    | VAR              ->     "VAR"
    | FUNCTION         ->     "FUNCTION"
    | BREAK            ->     "BREAK"
    | OF               ->     "OF"
    | END              ->     "END"
    | IN               ->     "IN"
    | NIL              ->     "NIL"
    | LET              ->     "LET"
    | DO               ->     "DO"
    | TO               ->     "TO"
    | FOR              ->     "FOR"
    | WHILE            ->     "WHILE"
    | ELSE             ->     "ELSE"
    | THEN             ->     "THEN"
    | IF               ->     "IF"
    | ARRAY            ->     "ARRAY"
    | ASSIGN           ->     "ASSIGN"
    | OR               ->     "OR"
    | AND              ->     "AND"
    | GE               ->     "GE"
    | GT               ->     "GT"
    | LE               ->     "LE"
    | LT               ->     "LT"
    | NEQ              ->     "NEQ"
    | EQ               ->     "EQ"
    | DIVIDE           ->     "DIVIDE"
    | TIMES            ->     "TIMES"
    | MINUS            ->     "MINUS"
    | PLUS             ->     "PLUS"
    | DOT              ->     "DOT"
    | RBRACE           ->     "RBRACE"
    | LBRACE           ->     "LBRACE"
    | RBRACK           ->     "RBRACK"
    | LBRACK           ->     "LBRACK"
    | RPAREN           ->     "RPAREN"
    | LPAREN           ->     "LPAREN"
    | SEMICOLON        ->     "SEMICOLON"
    | COLON            ->     "COLON"
    | COMMA            ->     "COMMA"
    | STRING content ->     "STRING:" ^ escape_string content
    | INT content -> "INT:" ^ string_of_int content
    | ID content  -> "ID:" ^ content
    | EOF -> "EOF"
