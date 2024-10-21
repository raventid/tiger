open Core
open Syntax

let revised_parser dispenser =
  MenhirLib.Convert.Simplified.traditional2revised
    Parser.main
    dispenser

let read_file filename : (unit ->
          Parser.token * Lexing.position *
          Lexing.position) list =
  let ic = In_channel.create filename in

  let rec read_tokens acc =
    match In_channel.input_line ic with
    | None ->
        In_channel.close ic;
        List.rev acc
    | Some line ->
        let token = parse line in
        read_tokens (
          (fun _ -> (Syntax.to_parser token, Lexing.dummy_pos, Lexing.dummy_pos)) :: acc
        )
  in

  read_tokens []

let parse_tokens tokens =
  let rec loop = function
    | [] -> ()
    | token :: rest ->
        revised_parser token;
        loop rest
  in
  try
    loop tokens;
    Ok () (* Parsing succeeded *)
  with
  | Parser.Error ->
      Error ("Dummy error message")
      (* Parsing failed, return error with position information *)

let main () =
  let filename = "~/Education/tiger/shared/lexed/test1.lexed" in
  let tokens = read_file filename in
  match parse_tokens tokens with
  | Ok () -> print_endline "Parsing successful"
  | Error (value) ->
      Printf.printf "Parsing error: %s" value

let _ = main ()
