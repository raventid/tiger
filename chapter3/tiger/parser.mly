/* Base keywords */
%token TYPE     "type"
%token VAR      "var"
%token FUNCTION "function"
%token BREAK    "break"
%token OF       "of"
%token END      "end"
%token IN       "in"
%token NIL      "nil" (* nil denotes a value belonging to every record type *)
%token LET      "let"
%token ARRAY    "array"

/* Loop keywords */
%token DO    "do"
%token TO    "to"
%token FOR   "for"
%token WHILE "while"

/* Conditional keywords */
%token IF   "if"
%token THEN "then"
%token ELSE "else"


/* Operators */

/* General */
%token ASSIGN ":="

/* Logical */
%token OR  "|"
%token AND "&"

/* Comparison */
%token GE  ">="
%token GT  ">"
%token LE  "<="
%token LT  "<"
%token NEQ "<>"
%token EQ  "="

/* Arithmetic operators */
%token DIVIDE "/"
%token TIMES  "*"
%token PLUS   "+"
%token MINUS  "-"

/* Separator tokens */
%token DOT       "."
%token LBRACE    "{"
%token RBRACE    "}"
%token LBRACK    "["
%token RBRACK    "]"
%token LPAREN    "("
%token RPAREN    ")"
%token SEMICOLON ";"
%token COLON     ":"
%token COMMA     ","

/* Misc tokens */
%token <string> STRING "string"
%token <int>    INT    "int"
%token <string> ID     "id"

%token EOF

%nonassoc "of"
%nonassoc "then"
%nonassoc "else"
%nonassoc "do"
%nonassoc ":="

%nonassoc ">=" ">" "<=" "<" "<>" "="
%left     "|" "&"
%left     "+" "-"
%left     "*" "/"

%start <unit> main

%%

let main :=
  ~ = expr; EOF; <>

/* EXPRESSIONS */
/* EVERYTHING RELATED TO EXPRESSIONS IN A LANGUAGE */

let expr :=
  | primitive
  | "nil"
  | "break"
  | create_rec
  | create_arr
  | lvalue
  | assignment
  | local
  | conditional
  | loop
  | fun_call
  | unary
  | binary
  | seq

/* Primitive values are Strings and Ints */
let primitive :=
  | "string"; { () }
  | "int"; { () }

/* Unary operators */
let unary := "-"; expr

/* bin - logical and arithmetic operators */
/* boolean - boolean logical operators */
let binary :=
  | bin
  | boolean

/* loop constructions: */
/* tiger language has while_loop and for_loop */
let loop :=
  | while_loop
  | for_loop

let while_loop := "while"; expr; "do"; expr
let for_loop   := "for"; "id"; ":="; expr; "to"; expr; "do"; expr

/* conditional expressions */
let conditional :=
  | "if"; expr; "then"; expr; "else"; expr
  | "if"; expr; "then"; expr

/* scope definition */
let local := "let"; decs; "in"; expr_seq; "end"

/* DECLARATIONS */
/* EVERYTHING RELATED TO DECLARATIONS */

let decs := list(dec); { () }

/* ty - type declaration */
/* var - value declaration */
/* fun - function declaration */
let dec :=
  | ty_dec
  | var_dec
  | fun_dec

let ty_dec := "type"; "id"; "="; ty

/* The type declaration could be a record, an array or a type alias (any identifier): */
let ty :=
  | braced(ty_fields)
  | "array"; "of"; "id"; { () }
  | "id"; { () }

let ty_fields := separated_list(",", ty_field); { () }
let ty_field := "id"; ":"; "id"

/* Variable definition could be with a type defined or without an explicit type definition */
let var_dec :=
| "var"; "id"; ":="; expr
| "var"; "id"; ":"; "id"; ":="; expr

/* Creating a record */
let create_rec := "id"; braced(init_rec_fields)
let init_rec_fields := separated_list(",", init_rec_field); { () }
let init_rec_field := "id"; "="; expr

/* Create a vector */
let create_arr := "id"; bracketed(expr); "of"; expr

/* Function declaration */
let fun_dec :=
   | "function"; "id"; parenthesized(ty_fields); expr
   | "function"; "id"; ":"; "id"; ":="; expr

/* LValue definition */
/* An l-value is a location, whose value may be read or assigned. */
/* Variables, procedure parameters, fields or records, and elements are all l-values. */
let lvalue :=
  (* variable *)
  | "id"; { () }
  (* everything else *)
  | lvalue_t

let lvalue_t :=
  (* simple record field *)
  | "id"; "."; "id"; { () }
  (* compound record field *)
  | lvalue_t; "."; "id"; { () }
  (* simple array subscript *)
  | "id"; bracketed(expr)
  (* compund array subscript *)
  | lvalue_t; bracketed(expr)

/* Assignment definition */
let assignment := lvalue; ":="; expr

/* Sequence of expressions */
let expr_seq := separated_list(";", expr); { () }
let seq := parenthesized(expr_seq)

/* Function call rule */
let fun_call := "id"; parenthesized(fun_args)
let fun_args := separated_list(",", expr); { () }

/* All the binary operators supported by my language */
let bin := expr; bin_op; expr
let bin_op == "+" | "-" | "*" | "/" | ">=" | ">" | "<=" | "<" | "<>" | "="

/*Boolean expressions*/
let boolean := expr; boolean_op; expr
let boolean_op == "&" | "|"

/* Helper grammar rules */
let parenthesized(x) == delimited("(", x, ")")
let bracketed(x)     == delimited("[", x, "]")
let braced(x)        == delimited("{", x, "}")
