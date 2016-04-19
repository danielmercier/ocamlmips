open Lexing
open Format

let usage = "usage: mk_bi [options] file"

let spec = []

let file =
  let file = ref None in
  let set_file s =
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let read_file sf =
  let ic = open_in_bin sf in
  let n = in_channel_length ic in
  let text = really_input_string ic n in
  close_in ic;
  text

let () =
  let text = read_file file in
  let lb = Lexing.from_string text in
  try
    let str = Parser.prog Lexer.token lb in
    print_string str;
    exit 0
  with
    | Error.Error (e,p) ->
      Error.print err_formatter file e p;
      exit 1
    | e ->
	    eprintf "Anomaly: %s\n@." (Printexc.to_string e);
      exit 2
