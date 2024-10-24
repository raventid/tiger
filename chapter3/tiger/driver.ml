open Core
open Syntax

let revised_parser dispenser =
  MenhirLib.Convert.Simplified.traditional2revised
    Parser.parse
    dispenser

let create_token_stream filename : (unit -> Parser.token * Lexing.position * Lexing.position) =
  let ic = In_channel.create filename in
  let line_no = ref 1 in

  printf "\n Opening file %s \n" filename;

  let next_token () =
    match In_channel.input_line ic with
    | None ->
      In_channel.close ic;
      (Parser.EOF, Lexing.dummy_pos, Lexing.dummy_pos)
    | Some line ->
      let token = parse line in
      printf "%s |  token: %s, line: %d \n" filename (show token) !line_no;
      incr line_no;
      (Syntax.to_parser token, Lexing.dummy_pos, Lexing.dummy_pos)
  in
  next_token

let with_test_files directory f =
  Sys_unix.readdir directory
  |> Array.iter ~f: (fun filename ->
      let full_filename = Filename.concat directory filename in
      f full_filename
    )

let directory = "./shared/lexed/"

let main () =
  let failed_to_parse_files = ref [] in
  with_test_files
    directory
    (fun filename ->
       try
         create_token_stream filename |> revised_parser
       with
         _ ->
         printf "Fatal: Cannot build AST from %s token stream \n" filename;
         failed_to_parse_files := filename :: !failed_to_parse_files
    );

  printf "\n\nFinished the execution of a test suite!\n";
  printf "Failed to parse next program samples: \n";
  List.iter !failed_to_parse_files (fun f -> printf "%s\n" f)

let _ = main ()
