open Base

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
  | Compound (stm1, stm2) ->
    let l1 = maxargs stm1 in
    let l2 = maxargs stm2 in
    Int.max l1 l2
  | Assign (_, exp) ->
    maxargs_exp exp

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
