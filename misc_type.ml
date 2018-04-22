open Token
open Rule
open Function_type
open Debug

exception Type_error of string
let fct_name:(string, string) Hashtbl.t = Hashtbl.create 10

let rec get_identifier = function
  | (Elem(Identifier(Name(v)), l)) :: t -> v
  | h :: t -> get_identifier t
  | _ -> raise (Type_error "idtf not found")

let rec get_scalar scalar = function
  | (Elem(Scalar(s), l)) :: t when scalar = s -> t
  | (Elem(r, l)) :: t ->
     begin
       match get_scalar scalar t with
       | [] -> get_scalar scalar l
       | e -> e
     end
  | _ -> []

let rec get_expr expr = function
  | (Elem(r, l)) :: t when r = expr -> l
  | (Elem(r, l)) :: t ->
     begin
       match get_expr expr t with
       | [] -> get_expr expr l
       | e -> e
     end
  | _ -> []

let change_const r v l t = function
  | "char" -> (Elem(Char_const(Integer(v)), l)) :: t
  | "int" -> (Elem(Integer_const(Integer(v)), l)) :: t
  | "bool" -> (Elem(Bool_const(Integer(v)), l)) :: t
  | _ -> (Elem(r, l)) :: t

let change_value ty v =
  let check = function
    | (t, ty) when compare t ty = 0 -> ()
    | (t, "Unk") -> Hashtbl.replace values v t; add_w_table (Hashtbl.find fct_name "")
    | ("char", "bool") -> Hashtbl.replace values v "bool"; add_w_table (Hashtbl.find fct_name "")
    | ("int", "bool") -> Hashtbl.replace values v "bool"; add_w_table (Hashtbl.find fct_name "")
    | ("int", "char") -> Hashtbl.replace values v "char"; add_w_table (Hashtbl.find fct_name "")

    | ("bool", "char") -> Hashtbl.replace values v "bool"; add_w_table (Hashtbl.find fct_name "")
    | ("bool", "int") -> Hashtbl.replace values v "bool"; add_w_table (Hashtbl.find fct_name "")
    | ("char", "int") -> Hashtbl.replace values v "char"; add_w_table (Hashtbl.find fct_name "")

    | (t, ty) -> raise (Type_error (v ^  ": " ^ t ^ " " ^ ty ^ " not compatible"))
  in try check (ty, Hashtbl.find values v)
     with | Not_found -> add_v_table (Hashtbl.find fct_name "") (v, ty)

(* back propagate type *)
let rec assign_type ty = function
  (* operator *)
  | (Elem(Type_binop(token, _), l)) :: t ->
     let left_n = List.nth l 0
     and right_n = List.nth l 1
     in (Elem(Type_binop(token, ty), assign_type ty [left_n] @ assign_type ty [right_n])) :: t
  (* call *)
  | (Elem(Postfix, l)) :: t when List.length l > 1 ->
     let idtf = get_identifier (get_expr Primary l) in
     let _ = replace_ret idtf (Hashtbl.find fct_name "") ty
     in (Elem(Postfix, l)) :: t

  (* For expr *)
  | (Elem(For_expr, l)) :: t -> (Elem(For_expr, l)) :: t

  (* variable *)
  | (Elem(Identifier(Name(v)), l)) :: t ->
     let _ = change_value ty v
     in (Elem(Identifier(Name(v)), l)) :: t

  | (Elem(Integer_const(Integer(v)), l)) :: t -> change_const (Integer_const(Integer(v))) v l t ty
  | (Elem(Char_const(Integer(v)), l)) :: t ->  change_const (Char_const(Integer(v))) v l t ty
  | (Elem(Double_const(Double(v)), l)) :: t -> (Elem(Double_const(Double(v)), l)) :: t
  | (Elem(r, l)) :: t ->
     let i = assign_type ty l
     in (Elem(r, i)) :: assign_type ty t
  | e -> e

and get_binop_type ty = function
  | More -> "bool"
  | Less -> "bool"
  | Pipe -> "bool"
  | And -> "bool"
  | _ -> ty

(* return type of a sequence *)
and get_type = function
  | (Elem(Integer_const(Integer(v)), l)) -> "int"
  | (Elem(Char_const(Integer(v)), l)) -> "char"
  | (Elem(Bool_const(Integer(v)), l)) -> "bool"
  | (Elem(Double_const(Double(v)), l)) -> "double"
  | (Elem(Identifier(Name(v)), l)) -> get_idtf_type (Hashtbl.find fct_name "") v

  | (Elem(Type_binop(tok, t), l)) -> get_binop_type t tok 

  | (Elem(Binop(tok), l)) -> get_binop_type "Unk" tok 

  (* ret call*)
  | (Elem(Postfix, l)) when List.length l > 1 ->
     let ret = function
       | (Elem(Primary, l)) :: t when t != [] -> get_ret (get_identifier l)
       | _ -> raise (Type_error "Nothing")
     in ret l

  | (Elem(Expressions, l)) when List.length l > 0 -> get_type (List.nth l ((List.length l) - 1))

  | (Elem(Paren_expr, l)) -> get_type (List.hd (get_scalar Open l))

  | (Elem(If_expr, l)) -> get_type (List.hd (get_expr Expressions l))

  | (Elem(For_expr, l)) -> get_type (List.hd (get_scalar In l))

  | (Elem(r, l)) when List.length l < 1 -> debug_ast [(Elem(r, l))]; raise (Type_error "Type unknown")

  | (Elem(r, l)) -> get_type (List.hd l)
  | _ -> raise (Type_error "Nothing")

(* set ret type *)
and ret_type ast =
  let rec check_error = function
    | ("Unk", ty) -> Hashtbl.replace values "" ty; ty
    | (v, "Unk") -> Hashtbl.replace values "" v; v

    | ("bool", "char") -> Hashtbl.replace values "" "bool"; "bool"
    | ("bool", "int") -> Hashtbl.replace values "" "bool"; "bool"
    | ("char", "bool") -> Hashtbl.replace values "" "bool"; "bool"
    | ("char", "int") -> Hashtbl.replace values "" "char"; "char"

    | ("int", "bool") -> Hashtbl.replace values "" "bool"; "bool"
    | ("int", "char") -> Hashtbl.replace values "" "char"; "char"

    | (v, ty) when v = ty -> Hashtbl.replace values "" ty; ty
    | (v, ty) -> raise (Type_error ("Return type error : " ^ v ^ " expected, but " ^ ty ^ " found"))

  and check = function
    | (Elem(Type_binop(tok, ty), l)) :: t ->
       let bin_ty = get_binop_type ty tok in
       let _ = check_error (Hashtbl.find values "", bin_ty) in
       (Elem(Type_binop(tok, ty), l)) :: t

    | (Elem(Unary, l)) :: t ->
       let ty = check_error (Hashtbl.find values "", get_type (List.hd l))
       in (Elem(Unary, assign_type ty l)) :: t

    | (Elem(r, l)) :: t -> (Elem(r, check l)) :: check t
    | _ -> []

  and expr_type =  function
    | (Elem(If_expr, l)) :: t ->
       let rec check = function
         | (Elem(Scalar(Then), l)) :: t -> (Elem(Scalar(Then), l)) :: ret_type t
         | (Elem(Scalar(Else), l)) :: t -> (Elem(Scalar(Else), l)) :: ret_type t
         | (Elem(r, l)) :: t -> (Elem(r, check l)) :: check t
         | _ -> []
       in (Elem(If_expr, check l)) :: t
    | (Elem(For_expr, l)) :: t ->
       let rec check = function
         | (Elem(Scalar(In), l)) :: t -> (Elem(Scalar(In), l)) :: ret_type t
         | (Elem(r, l)) :: t -> (Elem(r, check l)) :: check t
         | _ -> []
       in (Elem(For_expr, check l)) :: t

    | (Elem(Expression, l)) :: t -> (Elem(Expression, check l)) :: t

    | (Elem(r, l)) :: t -> (Elem(r, expr_type l)) :: expr_type t
    | _ -> []

  in match ast with
     | (Elem(Expressions, l)) :: t -> (Elem(Expressions, expr_type l)) :: ret_type t
     | (Elem(r, l)) :: t -> let i = ret_type l in (Elem(r, i)) :: ret_type t
     | _ -> []
