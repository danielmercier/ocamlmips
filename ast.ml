type ident = string

type lit =
  | Tint

type typ =
  | Tint
  | Tbool
  | Tstring
  | Tunit
  | Tarrow of typ * typ
  | Tproduct of typ list
  | Tvar of tvar
  | Tident of ident * typ (*Type définie par l'utilisateur*)
  | Tstruct (ident * field_typ) list (*Type structure*)

and tvar = { id : string; mutable def : typ option }

and field_type =
  | Fimmutable of typ
  | Fmuttable of typ

module V = struct
  type t = tvar
  let compare = Pervasives.compare
  let equal v1 v2 = v1.id = v2.id
  let create =
    let r = ref 0 in
    fun () ->
      incr r;
      { id = "t" ^ (string_of_int !r);
        def = None }
end

type const =
  | Cint of int
  | Cbool of bool
  | Cstring of string
  | Cunit

type pattern =
  | Pident of ident
  | Ptuple of pattern list

type 'info node_ident = {ident: ident; info: 'info}

type 'info node_expr = {expr: 'info expr; info: 'info}
and 'info expr =
  | Eident of ident
  | Econst of const
  | Efun of 'info node_ident * 'info node_expr
  | Eapp of 'info node_expr * 'info node_expr
  | Etuple of 'info node_expr list
  | Estruct  of ('info node_ident * 'info node_expr) list
  | Elet of 'info node_ident * 'info node_expr * 'info node_expr

type 'info instr =
  | Iexpr of 'info node_expr
  | Ilet of ident * 'info node_expr

type 'info prog = 'info instr list

type position = Lexing.position * Lexing.position
type typ_pos = { mutable otyp: typ option; pos: position }
type position_prog = position prog
type typ_pos_prog = typ_pos prog
