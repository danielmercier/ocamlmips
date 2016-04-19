open Algow
open Ast
open Error

module SSet = Set.Make(String)
type fenv = ident SSet.t
type senv = typ Smap.t
type tenv = {var: env; field: fenv; struc: senv}

let type_const c =
  match c with
  | Cint _    -> Tint
  | Cbool _   -> Tbool
  | Cstring _ -> Tstring
  | Cunit     -> Tunit

(*Check un type avec un type optionnel,
 * et le renvoie*)
let check_type t ot =
  match ot with
  | None -> t
  | Some t2 ->
      unify t2 t;
      t2

(*Check une expression qui doit être
 * une fonction et propage l'information a e*)
let check_fun efun e =
  match efun.info.otyp with
  | None ->
      Tvar (V.create ())
  | Some t ->
    begin
      match t with
      | Tarrow(t1, t2) ->
          let () =
            match e.info.otyp with
            | None -> e.info.otyp <- Some t2
            | _    -> ()
          in
          t1
      | _ -> error (Is_a_function (canon t)) efun.info.pos
    end

(* Crée un environnement avec l'environnement
 * typage var en paramètre *)
let mk_varenv var =
  { var=var;
  }

(*Typage des expressions*)
let rec w env expr =
  try
    match expr.expr with
    | Eident id     ->
      begin
        try
           check_type (find id env.vars) expr.info.otyp
        with
        | Not_found ->
            error (Unknown_identifier id) expr.info.pos
      end
    | Econst c      ->
        check_type (type_const c) expr.info.otyp
    | Efun (id, e)  ->
        let t_id = check_fun expr e in
        let new_env = mk_varenv (add id.ident t_id env.vars) in
        let tfun = Tarrow(t_id, w new_env e) in
        tfun
    | Eapp (e1, e2) ->
        let t_e1 = w env e1 in
        let t_e2 = w env e2 in
        let tn = Tvar (V.create ()) in
        begin
          try
            unify t_e1 (Tarrow (t_e2, tn))
          with
          | UnificationFailure (_, _, t1, t2) ->
              error (Type_error (t1, t2)) e2.info.pos
          | OccurCheck ->
              error Rectype expr.info.pos
        end;
        check_type tn expr.info.otyp
    | Etuple el ->
        Tproduct (List.map (w env) el)
    | Elet (id, e_let, e_in) ->
        let t_let = w env e_let in
        let new_env = mk_varenv (add_gen id.ident t_let env) in
        w new_env e_in
  with
  | UnificationFailure (to1, to2, t1, t2) ->
      error (Type_error (to1, to2)) expr.info.pos
  | OccurCheck ->
      error Rectype expr.info.pos

(* Typage des instructions *)
let type_instr i =
	match i with
  | Iexpr e -> w base_env e
  | _       -> failwith "todo"

(* Typage du programme *)
let type_prog p =
  List.map type_instr p
