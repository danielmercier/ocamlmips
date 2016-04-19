%{
  open Ast
  open Error

  let mk_node e startpos endpos =
    { expr = e;
      info = { pos=(startpos, endpos); otyp=None } }

  let mk_node_ident id otyp startpos endpos =
    { ident = id;
      info = { pos=(startpos, endpos); otyp=otyp } }


  let add_type e t =
    { expr = e.expr;
      info = {pos=e.info.pos; otyp=t } }

  let poltype = Hashtbl.create 80
%}

%token<string> IDENT
%token FUN
%token ARROW
%token LET
%token IN
%token LPAREN
%token RPAREN
%token COMMA
%token COLON
%token EQUAL
%token<int> CONST_INT
%token<bool> CONST_BOOL
%token<string> CONST_STRING
%token CONST_UNIT
%token END
%token EOI
%token MINUS
%token PLUS
%token STAR
%token SLASH
%token INT
%token BOOL
%token STRING
%token UNIT
%token APOST

%start prog
%type<Ast.typ_pos_prog> prog

%right ARROW

%nonassoc EXPR INSTR
%nonassoc LPAREN IDENT CONST_INT CONST_BOOL CONST_STRING CONST_UNIT
%left PLUS MINUS
%left STAR SLASH

%%
prog:
| li=eoi_instr* END
  { li }

eoi_instr:
| i=instr EOI?
    { i }

instr:
| e=expr %prec INSTR
    { Iexpr e }
| LET id=ident ot=ioption(COLON t=type_expr { t }) EQUAL e=expr %prec INSTR
    { Ilet(id, add_type e ot) }

expr:
| FUN id=ident ARROW e=expr %prec EXPR
    { let id = mk_node_ident id None $startpos(id) $endpos(id) in
      mk_node (Efun (id, e)) $startpos $endpos }
| FUN LPAREN id=ident COLON t=type_expr LPAREN ARROW e=expr %prec EXPR
    { let id = mk_node_ident id (Some t) $startpos(id) $endpos(id) in
      mk_node (Efun (id, e)) $startpos $endpos }
| e1=expr e2=simple_expr
    { mk_node (Eapp (e1, e2)) $startpos $endpos }
| LET id=ident ot=ioption(COLON t=type_expr { t }) EQUAL e1=expr IN e2=expr %prec EXPR
    { let id = mk_node_ident id ot ($startpos(id)) ($endpos(id)) in
      mk_node (Elet(id, e1, e2)) $startpos $endpos }
| e1=expr op=operator e2=expr
    { let fapp = mk_node (Eapp(op, e1)) $startpos $endpos in
      mk_node (Eapp (fapp, e2)) $startpos $endpos }
| e=simple_expr
    { e }

simple_expr:
| id=IDENT
    { mk_node (Eident id) $startpos $endpos }
| LPAREN op=operator RPAREN
    { op }
| c=const
    { mk_node (Econst c) $startpos $endpos }
| LPAREN e=expr COMMA l=separated_nonempty_list(COMMA, expr) RPAREN
    { mk_node (Etuple (e::l)) $startpos $endpos }
| LPAREN e=expr RPAREN
    { e }
| LPAREN e=expr COLON t=type_expr RPAREN
    { e.info.otyp <- Some t; e }

const:
| i = CONST_INT    { Cint i }
| b = CONST_BOOL   { Cbool b }
| s = CONST_STRING { Cstring s }
| CONST_UNIT       { Cunit }

ident:
| id=IDENT       { id }

%inline operator:
| PLUS           { mk_node (Eident("+")) $startpos $endpos }
| MINUS          { mk_node (Eident("-")) $startpos $endpos }
| STAR           { mk_node (Eident("*")) $startpos $endpos }
| SLASH          { mk_node (Eident("/")) $startpos $endpos }

type_expr:
| t=s_type_expr
  { Hashtbl.reset poltype;
    t }

s_type_expr:
| t1=simple_type_expr STAR tl=separated_nonempty_list(STAR, simple_type_expr)
    { Tproduct (t1::tl) }
| t1=s_type_expr ARROW t2=s_type_expr
    { Tarrow(t1, t2) }
| t=simple_type_expr                   { t }

simple_type_expr:
| INT                             { Tint }
| BOOL                            { Tbool }
| STRING                          { Tstring }
| UNIT                            { Tunit }
| APOST id=ident
    { try
        Hashtbl.find poltype id
      with Not_found ->
        let v = Tvar (V.create ()) in
        Hashtbl.add poltype id v;
        v }
| LPAREN t=s_type_expr RPAREN
    { t }
