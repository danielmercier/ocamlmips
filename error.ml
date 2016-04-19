open Ast
open Format
open Lexing

type error =
  | Lexical_error of string
  | Syntax_error of string
  | Interpretation_error
  | Structure_identifier of string
  | Function_identifier of string
  | Unknown_identifier of string
  | Not_a_function
  | Not_a_struct_type of typ
  | Not_an_option of typ
  | Field_not_found of ident * typ
  | Unknown_field_name of ident
  | Unknown_type of ident
  | Not_mutable of ident
  | Different_identifiers of ident * ident
  | Type_error of typ * typ
  | Rectype
  | Is_a_function of typ

exception Error of error * Ast.position

let report_loc fmt file (b,e) =
  if b = dummy_pos || e = dummy_pos then
  fprintf fmt "File \"%s\"\nerror: " file
  else
  let l = b.pos_lnum in
  let fc = b.pos_cnum - b.pos_bol + 1 in
  let lc = e.pos_cnum - b.pos_bol + 1 in
  fprintf fmt "File \"%s\", line %d, characters %d-%d\nerror: " file l fc lc

let tvar_to_string = string_of_int

let typ_to_string t =
  let st = function
    | Tarrow _ -> false
    | Tproduct _ -> false
    | _ -> true
  in
  let env = Hashtbl.create 80 in
  let poly = ref (-1) in
  let rec typ_to_string t =
    match t with
      | Tunit       -> "unit"
      | Tbool       -> "bool"
      | Tint        -> "int"
      | Tstring     -> "string"
      | Tarrow (t1, t2) ->
          let ts1 = typ_to_string t1 in
          let ts2 = typ_to_string t2 in
          let ts1 = if st t1 then ts1 else "(" ^ ts1 ^ ")" in
           ts1 ^ " -> " ^ ts2
      | Tproduct lt ->
          begin
            match lt with
            | h :: q ->
                let tstr t =
                  if st t then
                    typ_to_string t
                  else "(" ^ typ_to_string t ^ ")"
                in
                List.fold_left
                  (fun acc t ->
                    acc ^ " * " ^ tstr t)
                  (tstr h)
                  q

            | _ -> assert false
          end
      | Tvar tv ->
          let var =
            try
              Hashtbl.find env tv.id
            with Not_found ->
              incr poly;
              Hashtbl.add env tv.id !poly;
              !poly
          in
          let c = Char.chr (var + Char.code 'a') in
          "'" ^ (Char.escaped c)
  in
  typ_to_string t

let to_string e =
  match e with
    | Lexical_error s -> sprintf "lexical error: %s" s
    | Syntax_error s -> sprintf "syntax error: %s" s
    | Interpretation_error -> sprintf "interpretation error"
    | Structure_identifier id -> sprintf "identifier %s denotes a structure type" id
    | Function_identifier id -> sprintf "identifier %s denotes a function" id
    | Unknown_identifier id -> sprintf "unknown identifier %s" id
    | Not_a_function -> sprintf "this is not a function, it cannot be applied"
    | Not_an_option typ -> sprintf "option type expected, which %s is not" (typ_to_string typ)
    | Not_a_struct_type typ -> sprintf "structure type expected, which %s is not" (typ_to_string typ)
    | Field_not_found (id, typ) -> sprintf "field %s not found in structure type %s" id (typ_to_string typ)
    | Unknown_field_name id -> sprintf "unknown field name %s" id
    | Unknown_type id -> sprintf "unknown type name %s" id
    | Not_mutable id -> sprintf "immutable field %s" id
    | Different_identifiers(id1, id2) -> sprintf "identifiers %s and %s do not match" id1 id2
    | Type_error(t1, t2) -> sprintf "this expression should have type %s but has type %s" (typ_to_string t1) (typ_to_string t2)
    | Rectype -> sprintf "this expression have a recursive type"
    | Is_a_function t -> sprintf "this expression should not be a function, the expected type is %s" (typ_to_string t)

let print_i fmt f e p =
  let rec nc c i =
    if i > 0 then
      c ^ (nc c (i-1))
    else
      ""
  in
  let (bp, ep) = p in
  let fc = bp.pos_cnum - bp.pos_bol in
  let lc = ep.pos_cnum - bp.pos_bol in
  print_string (nc " " fc);
  print_string (nc "^" (lc - fc));
  print_newline ();

  fprintf fmt "error: %s\n@." (to_string e)

let print fmt f e p =
  report_loc fmt f p;
  fprintf fmt "%s\n@." (to_string e)

let error e p = raise (Error (e,p))
