
(* The type of tokens. *)

type token = 
  | VAL
  | UNIT
  | STRING
  | STAR
  | RPAREN
  | LPAREN
  | INT
  | IDENT of (string)
  | END
  | COLON
  | BOOL
  | ARROW
  | APOST

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val prog: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (string)
