{
  open Lexing
  open Parser
  open Error
  open Format

  let current_pos b =
    lexeme_start_p b,
    lexeme_end_p b

  let id_or_keyword =
    let h = Hashtbl.create 17 in
    List.iter (fun (s, k) -> Hashtbl.add h s k)
      [
	      "true",          CONST_BOOL(true);
	      "false",         CONST_BOOL(false);
	      "let",           LET;
	      "in",            IN;
        "fun",           FUN;
        "int",           INT;
        "bool",          BOOL;
        "string",        STRING;
        "unit",          UNIT
      ];
    fun s ->
      try Hashtbl.find h s with Not_found -> IDENT s

  (*strbuf est utile pour read_string, lire une chaine entre guillemet*)
  let strbuf = Buffer.create 80
}


let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | '\'' | digit)*

rule token = parse
  | '\n'
      { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+
      { token lexbuf }
  | "(*"
      { comment lexbuf; token lexbuf }

  | digit+
      { CONST_INT (int_of_string (lexeme lexbuf)) }
  | ident
      { id_or_keyword (lexeme lexbuf) }
  | '"'
      { Buffer.reset strbuf;
        read_string (current_pos lexbuf) lexbuf;
        CONST_STRING (Buffer.contents strbuf) }
  | "()" { CONST_UNIT }

  | "->"
      { ARROW }

  | "("
      { LPAREN }
  | ")"
      { RPAREN }
  | ":"
      { COLON }
  | "'"
      { APOST }
  | ","
      { COMMA }

  | "-"
      { MINUS }
  | "+"
      { PLUS }
  | "*"
      { STAR }
  | "/"
      { SLASH }

  | "="
      { EQUAL }

  | _
      { error (Lexical_error (lexeme lexbuf)) (current_pos lexbuf) }

  | ";;"
      { EOI }
  | eof
      { END }

and comment = parse
  | "(*"
      { comment lexbuf; comment lexbuf }
  | "*)"
      { () }
  | _
      { comment lexbuf }
  | eof
      { error (Lexical_error "unterminated comment") (current_pos lexbuf) }

and read_string pos = parse
  | "\\n" { Buffer.add_char strbuf '\n'; read_string pos lexbuf }
  | "\\t" { Buffer.add_char strbuf '\t'; read_string pos lexbuf }
  | "\\\"" { Buffer.add_char strbuf  '"'; read_string pos lexbuf }
  | "\\\\" { Buffer.add_char strbuf '\\'; read_string pos lexbuf }
  | '"' { () }
  | [' '-'~'] as c { Buffer.add_char strbuf c; read_string pos lexbuf }
  | eof {
      error
        (Lexical_error "String not terminated")
        (current_pos lexbuf)
  }
  | _ as c
      { error
          (Lexical_error
            (sprintf "Illegal character '%c' in string" c))
          (current_pos lexbuf) }

{
  let buffer = ref ErrorReports.Zero

  let lexer lb =
    let tok = token lb in
    buffer := ErrorReports.update !buffer (current_pos lb);
    tok
}
