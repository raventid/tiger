open Core_kernel
open Lexing

type loc = position * position

(* location helper *)
type 'a loc_t =
  { value: 'a;
    loc: loc [@opaque];
  } [@@deriving show]

let mk value loc = { value; loc }

let dummy value = mk value (dummy_pos, dummy_pos)

let range_string { loc; _ } =
  let (pos_start, pos_end) = loc in
  let line_start = pos_start.pos_lnum in
  let line_end = pos_end.pos_lnum in
  let col_start = pos_start.pos_cnum - pos_start.pos_bol in
  let col_end = pos_end.pos_cnum - pos_end.pos_bol in
  Printf.sprintf "(%d,%d)-(%d,%d)"
    line_start col_start line_end col_end


(* symbol helper *)
type symbol_t = int * string [@@deriving show]

let symbol =
  let tbl = Hashtbl.create (module String) in
  let idx = ref (-1) in
  fun key ->
    match Hashtbl.find tbl key with
    | Some x -> x, key
    | None ->
      incr idx;
      Hashtbl.add_exn tbl ~key ~data:!idx;
      !idx, key

let name = snd


type op =
  (* arithmetics *)
  | Plus
  | Minus
  | Times
  | Divide
  (* comparison *)
  | Ge
  | Gt
  | Le
  | Lt
  | Eq
  | Neq
  [@@deriving show]

type field = {
  name : symbol_t loc_t;
  typ : symbol_t loc_t;
  escape : bool ref;
} [@@deriving show]

(* Type *)
type ty =
  | NameTy of symbol_t loc_t
  | RecordTy of field list
  | ArrayTy of symbol_t loc_t
  [@@deriving show]

type expr =
  | Var of var loc_t
  | Nil of unit loc_t
  | Int of int loc_t
  | String of string loc_t
  | Call of symbol_t loc_t * (* name *)
            expr loc_t list (* args *)
  | Op of expr loc_t * (* left operand *)
          op loc_t * (* operator *)
          expr loc_t (* right operand *)
  | Record of symbol_t loc_t * (* name *)
              (symbol_t loc_t * expr loc_t) list (* fields *)
  | Seq of expr loc_t list
  | Assign of var loc_t *
              expr loc_t
  | If of expr loc_t * (* condition *)
          expr loc_t * (* then *)
          expr loc_t option (* else *)
  | While of expr loc_t * (* condition *)
             expr loc_t (* body *)
  | For of symbol_t * (* iterator name *)
           expr loc_t * (* from *)
           expr loc_t * (* to *)
           expr loc_t * (* body *)
           bool ref (* escape *)
  | Break of unit loc_t
  | Let of dec list * (* declarations *)
           expr loc_t (* body *)
  | Array of symbol_t loc_t * (* type *)
             expr loc_t * (* size *)
             expr loc_t (* init *)
  [@@deriving show]

(* Variable *)
and var =
  | SimpleVar of symbol_t loc_t
  | FieldVar of var loc_t * symbol_t loc_t
  | SubscriptVar of var loc_t * expr loc_t
  [@@deriving show]

(* Type, value or function declaration *)
and dec =
  | TypeDec of type_dec loc_t
  | VarDec of var_dec loc_t
  | FunDec of fun_dec loc_t
  [@@deriving show]

(* Value declaration *)
and var_dec = {
  var_name : symbol_t loc_t;
  var_typ : symbol_t loc_t option;
  init : expr loc_t;
  escape : bool ref;
} [@@deriving show]

(* Type declaration *)
and type_dec = {
  type_name : symbol_t loc_t;
  typ : ty;
} [@@deriving show]

(* Function declaration *)
and fun_dec = {
  fun_name : symbol_t loc_t;
  params : field list;
  body : expr loc_t;
  result_typ : symbol_t loc_t option;
} [@@deriving show]
