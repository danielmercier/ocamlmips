open Typing
open Type.Type
open Type
open Error
open Format

let print_unified t1 t2 =
  let s = unify t1 t2 in
  let str = sprintf "Type t1: %s\nType t2: %s\n\n" (typ_to_string (apply s t1)) (typ_to_string (apply s t2)) in
  print_string str

let print_types t1 t2 =
  let str = sprintf "Type t1: %s\nType t2: %s\n\n" (typ_to_string t1) (typ_to_string t2) in
  print_string str

let print t1 t2 =
  print_types t1 t2;
  print_unified t1 t2;
  print_string "=====================================\n"

let () =
  let create () = Tvar (V.create ()) in
  let t1 = Tint in
  let t2 = create () in
  let () = print t1 t2 in

  let t1 = Tarrow(Tint, Tbool) in
  let tv = create () in
  let t2 = Tarrow(tv, tv) in
  let () = print t1 t2 in
  ()
