open Ast

(*===============NORMALISATION===============*)
(* On suit les définition multiple des type var
 * mais uniquement pour t, pas récursivement*)
let rec head t =
  match t with
  | Tvar v ->
    begin
      match v.def with
      | Some t -> head t
      | None -> t
    end
  | _ -> t

(* forme canonique, forme en suivant tout les type
 * variable récursivement, appel de head recursivement *)
let rec canon t =
  match head t with
  | Tarrow (t1, t2) -> Tarrow (canon t1, canon t2)
  | Tproduct lt     -> Tproduct (List.map canon lt)
  | t -> t
(*===========================================*)

(*================UNIFICATION================*)
exception UnificationFailure of typ * typ * typ * typ
exception OccurCheck

let unification_error to1 to2 t1 t2 = raise (UnificationFailure (canon to1, canon to2, canon t1, canon t2))
let occur_error () = raise OccurCheck

(* Test si tvar a une occurence dans t *)
let rec occur tvar t =
  match head t with
  | Tarrow (t1, t2) -> occur tvar t1 || occur tvar t2
  | Tproduct tl     -> List.exists (occur tvar) tl
  | Tvar v          -> V.equal tvar v
	| _								-> false

(* Unification des type t1 et t2 *)
let unify t1 t2 =
  let (to1, to2) = (t1, t2) in
  let rec unify t1 t2 =
    match (head t1, head t2) with
    | (t1, t2) when t1 = t2 -> ()
    | (Tarrow(t1, tp1), Tarrow(t2, tp2)) ->
        unify t1 t2;
        unify tp1 tp2
    | (Tproduct tl1, Tproduct tl2) ->
        List.iter2 unify tl1 tl2
    | (Tident (id1, t1), Tident (id2, t2) when id1 = id2 -> ()
    | (Tident (id1, t1), Tident (id2, t2) ->
        unify t1 t2
    | (Tvar v, t) ->
        if occur v t then
          occur_error ();
        v.def <- Some t
    | (t, Tvar v) ->
        unify (Tvar v) t
    (* Les types structures ne peuvent être égale que si leurs id le sont *)
    | (t1, t2) -> unification_error to1 to2 t1 t2
  in
  unify t1 t2
(*===========================================*)

(*=================VARIABLES=================*)
module Vset = Set.Make(V)

(*Ensemble des variables libres de t*)
let rec fvars t =
	match head t with
	| Tarrow(t1, t2) -> Vset.union (fvars t1) (fvars t2)
	| Tproduct tl    -> List.fold_left Vset.union Vset.empty (List.map fvars tl)
	| Tvar v				 -> Vset.singleton v
  | _              -> Vset.empty
(*===========================================*)

(*==============ENVIRONNEMENT================*)
type schema = { vars : Vset.t; typ : typ }

module Smap = Map.Make(String)

type env = { bindings : schema Smap.t; fvars : Vset.t }

let empty = { bindings = Smap.empty; fvars = Vset.empty }

let update env =
  let new_fvars =
    Vset.fold
      (fun v -> Vset.union (fvars (Tvar v)))
      env.fvars
      Vset.empty
  in
  new_fvars

let add id typ env =
  let schema = { vars = Vset.empty; typ = typ } in
  let new_bind = Smap.add id schema env.bindings in
  { bindings = new_bind;
    fvars = update env }

let add_gen id typ env =
  let free_env_vars =
    Smap.fold
      (fun _ { typ = typ } ->
        Vset.union ( fvars typ ))
      env.bindings
      Vset.empty
  in
  let free_vars = Vset.diff (fvars typ) free_env_vars in
  let schema = { vars = free_vars; typ = typ } in
  let new_bind = Smap.add id schema env.bindings in
  { bindings = new_bind;
    fvars = update env }

let find id env =
  let module Vmap = Map.Make(V) in
  let {vars = vars ; typ = typ } = Smap.find id env.bindings in
  let subs =
    Vset.fold
      (fun v -> Vmap.add v (Tvar (V.create ())))
      vars
      Vmap.empty
  in
  let rec apply t =
    match head t with
    | Tarrow(t1, t2)              -> Tarrow (apply t1, apply t2)
    | Tproduct tl                 -> Tproduct (List.map apply tl)
    | Tvar v when Vmap.mem v subs -> Vmap.find v subs
    | t                           -> t
  in
  apply typ

(* On ajoute les fonctions de base *)
let base_env =
  List.fold_left
    (fun env (id, typ) ->
      add_gen id typ env)
    empty
    Built_in.built_in
(*===========================================*)
