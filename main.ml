open Parser.MenhirInterpreter
open Lexing
open ErrorReports
open Error
open Format

let usage = "usage: compilo [options] file.ml"

let interpret = ref false

let spec =
  ["-i", Arg.Set interpret, "interpret mode";
]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".ml") then
      raise (Arg.Bad "no .ml extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with
  | Some f -> f
  | None -> if not !interpret then (interpret := true; "") else ""

let read_file sf =
  let ic = open_in_bin sf in
  let n = in_channel_length ic in
  let text = really_input_string ic n in
  close_in ic;
  text

let () =
  let () = if !interpret then (Interpret.interpret (); exit 0) in
  let text = read_file file in
  let lb = Lexing.from_string text in
  try
    let module I = Parser.MenhirInterpreter in
    let checkpoint = Parser.Incremental.prog lb.lex_curr_p
    and supplier = I.lexer_lexbuf_to_supplier Lexer.lexer lb
    and succeed p =
      let lst = Typing.type_prog p in
      List.iter
        (fun t ->
          print_string (typ_to_string (Algow.canon t));
          print_newline ())
        lst
    and fail checkpoint =
      let (pos, s) = report text !Lexer.buffer checkpoint in
      error (Syntax_error s) pos
    in
    let _ = I.loop_handle succeed fail supplier checkpoint in
    exit 0
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
