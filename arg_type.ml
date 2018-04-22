open Token
open Rule
open Function_type
open Misc_type

(* ARGS MANIPULATION *)
let rec get_proto fct = function
  | (Elem(Prototype_args, l)) :: t -> (Elem(Prototype_args, fct l)) :: t
  | (Elem(r, l)) :: t -> (Elem(r, get_proto fct l)) :: get_proto fct t
  | _ -> []
(* get type declaration *)
and type_arg = function
  | (Elem(Type(Name(v)), _)) :: t -> v
  | (Elem(Identifier(_), _)) :: t -> "Unk"
  | (Elem(Scalar(Close), _)) :: t -> "Unk"
  | h :: t -> type_arg t
  | _ -> "Unk"
(* change args *)
and change_args ast =
  let rec change_a = function
    | (Elem(Identifier(Name(v)), _)) :: t -> (Elem(Arg(v, type_arg t), [])) :: change_a t
    | (Elem(Scalar(Close), _)) :: t -> (Elem(Arg("", type_arg t), [])) :: change_a t
    | (Elem(r, l)) :: t -> change_a t
    | _ -> []
  in get_proto change_a ast
(* actualize prototype *)
and change_proto ast =
  let rec change_p = function
    | (Elem(Arg(v, ty), _)) :: t -> (Elem(Arg(v, Hashtbl.find values v), [])) :: change_p t
    | (Elem(r, l)) :: t -> (Elem(r, change_p l)) :: change_p t
    | _ -> []
  in get_proto change_p ast
(* activate values table *)
and act_table name ast =
  let _ = Hashtbl.clear values
  and _ = Hashtbl.replace fct_name "" name
  and f = Hashtbl.find f_table name
  and args = get_args ast in
  let rec check = function
    | ((v, ty) :: t, f :: tf) -> Hashtbl.add values v f; check (t, tf)
    | _ -> []
  in check (args, f)

(* get list of args *)
and get_args def =
  let rec get_a = function
    | (Elem(Arg(v, ty), _)) :: t -> (v, ty) :: get_a t
    | h :: t -> get_a t
    | _ -> []
  in match def with
     | (Elem(Prototype_args, l)) :: t -> get_a l
     | (Elem(r, l)) :: t -> (get_args l) @ (get_args t)
     | _ -> []
