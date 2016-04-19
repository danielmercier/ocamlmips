%{
  open Error

  let declare id t =
    "\t(\"" ^ id ^ "\"," ^ t ^ ");\n"

  let env = Hashtbl.create 80
  let cptmorph = ref 0

  let mk_tvar i =
    Printf.sprintf "(Tvar {id = \"b%s\"; def = None} )" (string_of_int i)
%}

%token INT BOOL STRING UNIT
%token<string> IDENT
%token LPAREN RPAREN COLON
%token ARROW STAR APOST
%token VAL
%token END

%right ARROW

%start prog
%type<string> prog

%%

prog:
  | ld=declaration* END
      { let declarations =
          List.fold_left
            (fun str t -> str ^ t)
            ""
            ld
        in
        "open Ast\n\n" ^
        "let built_in = [\n" ^
        declarations ^ "]\n" }
  | error { error Syntax_error ($startpos, $endpos) }

declaration:
  | VAL id=IDENT COLON t=type_expr { Hashtbl.reset env; declare id t }

type_expr:
  | INT { "Tint" }
  | BOOL { "Tbool" }
  | STRING { "Tstring" }
  | UNIT { "Tunit" }
  | APOST id=IDENT
      { let i =
          try
            Hashtbl.find env id
          with Not_found ->
            incr cptmorph;
            Hashtbl.add env id !cptmorph;
            !cptmorph
        in
        mk_tvar i }
  | LPAREN t1=type_expr STAR lt=separated_nonempty_list(STAR, type_expr) RPAREN
      { let str=
          List.fold_left
            (fun str t -> str ^ ";" ^ t)
            t1
            lt
          in
        "(Tproduct([" ^ str ^ "]))" }

  | t1=type_expr ARROW t2=type_expr { "(Tarrow(" ^ t1 ^ "," ^ t2 ^ "))" }
  | LPAREN t=type_expr RPAREN       { t }
