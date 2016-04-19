open Parser.MenhirInterpreter
open Lexing
open ErrorReports
open Error
open Format

let usage = "usage: compilo [options] file.ml"

let interpret_line () =
  let text = input_line stdin in
  let lb = Lexing.from_string text in
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
  I.loop_handle succeed fail supplier checkpoint

let interpret () =
  while true do
    try
      interpret_line ();
      print_newline ()
    with
      | Error.Error (e,p) ->
        Error.print_i err_formatter "" e p;
      | End_of_file -> exit 0
      | e ->
        eprintf "Anomaly: %s\n@." (Printexc.to_string e);
  done
