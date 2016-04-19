{
  open Lexing
  open Parser
  open Format

  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
        "int",           INT;
        "bool",          BOOL;
        "string",        STRING;
        "unit",          UNIT;
        "val",           VAL;
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s
}

let ident = [^'\n''\r''\t'':''-''>''<''\''' '';''('')''*']+

rule token = parse
  | '\n'             { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+ { token lexbuf }
  | ident as id      { id_or_keyword id }
  | "->"             { ARROW }
  | "*"              { STAR }
  | "("              { LPAREN }
  | ")"              { RPAREN }
  | ":"              { COLON }
  | "'"              { APOST }
  | eof              { END }
