open Core
open Syntax

let construct_parser =
  MenhirLib.Convert.Simplified.traditional2revised Parser.parse

let revised_parser dispenser = construct_parser dispenser

let read_file filename : (
          unit ->
          Parser.token * Lexing.position * Lexing.position
) list =
  let ic = In_channel.create filename in

  let rec read_tokens acc line_no =
    match In_channel.input_line ic with
    | None ->
        In_channel.close ic;
        List.rev acc
    | Some line ->
        let token = parse line in
        let position = {
          Lexing.pos_fname = "dummy.lexed";
          Lexing.pos_lnum = line_no;
          Lexing.pos_bol = 0;
          Lexing.pos_cnum = 0; } in
        print_endline (show token);
        read_tokens
          ((fun _ -> (Syntax.to_parser token, position, Lexing.dummy_pos) :: acc)
          (line_no + 1)
  in
  read_tokens [] 1

let parse_tokens tokens =
  let rec loop = function
    | [] -> ()
    | token :: rest ->
        revised_parser token;
        loop rest
  in
  (* try *)
    loop tokens;
    Ok () (* Parsing succeeded *)
  (* with *)
  (* | Parser.Error -> *)
      (* Error ("Dummy error message") *)

let main () =
  let filename = "/Users/raventid/Education/tiger/shared/lexed/nil.lexed" in
  let tokens = read_file filename in
  printf "Amount of tokens: %d" (List.length tokens);
  match parse_tokens tokens with
  | Ok () -> print_endline "No grammar violations detected"
  | Error (value) ->
      Printf.printf "Parsing error: %s" value

let _ = main ()
