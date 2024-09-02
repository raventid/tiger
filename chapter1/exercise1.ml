open Base
open Straightline

type id = string

type binop = Plus | Minus | Times | Div

type stm = Compound of stm * stm
         | Assign of id * exp
         | Print of exp list
and exp = Id of id
         | Num of int
         | Op of exp * binop * exp
         | Eseq of stm * exp

(* This function tells the maximum number of args *)
(* of any print statement *)
(* within any subexpression of a given statement. *)
let rec maxargs stm =
  let maxargs_exp = function
    | Eseq (stm, _) -> maxargs stm
    | _ -> 0
  in match stm with
  | Print exps ->
    let len = List.length exps in
    let exp_lens = List.map ~f:maxargs_exp exps in
    let max_exp_len = List.max_elt ~compare:Int.compare exp_lens in
    Int.max len (Option.value ~default:0 max_exp_len)
  | Compound (exp1, exp2) ->
    let l1 = maxargs exp1 in
    let l2 = maxargs exp2 in
    Int.max l1 l2
  | Assign (_, exp) ->
    maxargs_exp exp

(* TODO: Intepretation function *)
let interp stm = interp_stm stm init_env

let rec interp_stm stm env =
  match stm with
  | Compound (stm1, stm2) -> failwith "Invalid operation or operands"
  | Assign (id, exp) -> failwith "Invalid operation or operands"
  | Print expressions ->

and interp_exp exp env =
  match exp with
  | Id id -> failwith "Invalid operation or operands"
  | Num int -> int
  | Op (exp1, binop, exp2) -> (
    match binop with
    | Plus -> (interp_exp exp1 env) + (interp_exp exp2 env)
    | Minus -> (interp_exp exp1 env) - (interp_exp exp2 env)
    | Times -> (interp_exp exp1 env) + (interp_exp exp2 env)
    | Div -> (interp_exp exp1 env) + (interp_exp exp2 env))
  | Eseq (stm, exp) -> failwith "Invalid operation or operands"

let init_env = let env = Hashtbl.create 16 in env

let unit_test_interp = ( interp Print(Op(Num 10, Plus, Num 10)) ) == 20

(* Example AST represantation in Ocaml *)
let stm = Compound(
  Assign("a", Op(Num 5, Plus, Num 3)),
  Compound(
    Assign("b",
      Eseq(
        Print [Id "a"; Op(Id "a", Minus, Num 1)],
        Op(Num 10, Times, Id "a"))),
    Print [Id "b"]))

let main = maxargs stm

main;;
