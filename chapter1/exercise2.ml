open Base
open Stdio

type id = string

type binop = Plus | Minus | Times | Div

type stm = Compound of stm * stm
         | Assign of id * exp
         | Print of exp list
and exp = Id of id
         | Num of int
         | Op of exp * binop * exp
         | Eseq of stm * exp

let rec interp_stm stm env =
  match stm with
  | Compound (stm1, stm2) ->
      let env' = interp_stm stm1 env in
      interp_stm stm2 env'
  | Assign (id, exp) ->
      let (v, env') = interp_exp exp env in
      Map.set env' ~key:id ~data:v
  | Print expressions ->
      List.fold expressions ~init:env ~f:(fun env exp ->
        let (v, env') = interp_exp exp env in
        printf "%d\n" v;
        env'
      )
and interp_exp exp env =
  match exp with
  | Id id ->
      (Map.find env id |> function
       | Some v -> (v, env)
       | None -> failwith ("Unbound variable: " ^ id))
  | Num number -> (number, env)
  | Op (exp1, binop, exp2) ->
      let (v1, env') = interp_exp exp1 env in
      let (v2, env'') = interp_exp exp2 env' in
      let result = match binop with
        | Plus -> v1 + v2
        | Minus -> v1 - v2
        | Times -> v1 * v2
        | Div -> v1 / v2
      in
      (result, env'')
  | Eseq (stm, exp) ->
      let env' = interp_stm stm env in
      interp_exp exp env'

let init_env = Map.empty(module String)

let interp stm = interp_stm stm init_env

(* To run the test in utop *)
(* test ep;; *)
let test stm =
  let final_env = interp_stm stm init_env in
  let expected_env = Map.of_alist_exn (module String) [("a", 8); ("b", 80)] in
  Map.equal Int.equal final_env expected_env

(* Example AST representation in OCaml *)
let example_program = Compound(
  Assign("a", Op(Num 5, Plus, Num 3)),
  Compound(
    Assign("b",
      Eseq(
        Print [Id "a"; Op(Id "a", Minus, Num 1)],
        Op(Num 10, Times, Id "a"))),
    Print [Id "b"]))

(*Shortcut for utop*)
let ep = example_program;;

interp ep;;
