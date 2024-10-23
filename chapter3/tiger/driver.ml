open Core
open Syntax

let revised_parser dispenser =
  MenhirLib.Convert.Simplified.traditional2revised
    Parser.parse
    dispenser

let create_token_stream filename : (unit -> Parser.token * Lexing.position * Lexing.position) =
  let ic = In_channel.create filename in
  let line_no = ref 1 in

  let rec next_token () =
    match In_channel.input_line ic with
    | None ->
        In_channel.close ic;
        (Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
    | Some line ->
        let token = parse line in
        printf "Current token: %s, line: %d \n" (show token) !line_no;
        incr line_no;
        (Syntax.to_parser token, Lexing.dummy_pos, Lexing.dummy_pos)
  in

  next_token

let run_revised_parser dispenser =
  Ok ()

let main () =
  let filename = "/Users/raventid/Education/tiger/shared/lexed/nil.lexed" in
  let stream = create_token_stream filename in
    revised_parser stream

let _ = main ()
