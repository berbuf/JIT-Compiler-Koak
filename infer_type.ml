open Token
open Rule
open Function_type
open Misc_type

(* INFER TYPES *)
let rec infer_type l =

  (* infer call *)
  let rec infer_call unops l =
    let rec get_call_types acc = function
      | (Elem(Expression, l)) :: t -> get_call_types (get_type (List.hd l) :: acc) t
      | h :: t -> get_call_types acc t
      | _ -> List.rev acc

    and back_propa = function
      | ((Elem(Expression, l)) :: t, ty :: tty) ->
         let lty = assign_type ty l
         in (Elem(Expression, lty)) :: back_propa (t, tty)
      | ((Elem(r, l)) :: t, ty) -> (Elem(r, back_propa (l, ty))) :: back_propa (t, ty)
      | _ -> []

    and name = get_identifier (get_expr Primary l)
    and p = infer_type l in
    let c = get_call_types [] (get_expr Call_expr p) in
    let _ = replace_signature name (Hashtbl.find fct_name "") c

    and b = back_propa (p, (get_sign name))
    in (Elem(Postfix, b)) :: unops

  (* infer binop *)
  and infer_binop token l =
    let get_node = function
      | (Elem(r, l)) -> List.hd (infer_type [(Elem(r, l))])
      |  e -> e
    in
    let create_binop token t left_node right_node =
      (Elem(Type_binop(token, t), infer_type [left_node; right_node])) :: []
    and left_node = get_node (List.nth l 0)
    and right_node = get_node (List.nth l 1) in
    
    let match_type _ =
      let l_ty = get_type left_node
      and r_ty = get_type right_node
      in match (l_ty, r_ty) with
         | ("Unk", "Unk") -> create_binop token "Unk" left_node right_node
         | ("bool", "char") -> create_binop token "bool" left_node (List.hd (assign_type "bool" [right_node]))
         | ("bool", "int") -> create_binop token "bool" left_node (List.hd (assign_type "bool" [right_node]))
         | ("char", "bool") -> create_binop token "bool" (List.hd (assign_type "bool" [left_node])) right_node
         | ("char", "int") -> create_binop token "char" left_node (List.hd (assign_type "char" [right_node]))
         | ("int", "bool") -> create_binop token "bool" (List.hd (assign_type "bool" [left_node])) right_node
         | ("int", "char") -> create_binop token "char" (List.hd (assign_type "char" [left_node])) right_node
         | ("Unk", t2) -> create_binop token t2 (List.hd (assign_type t2 [left_node])) right_node
         | (t1, "Unk") -> create_binop token t1 left_node (List.hd (assign_type t1 [right_node]))
         | (t1, t2) when t1 = t2 -> create_binop token t1 left_node right_node
         | (a, b) -> raise (Type_error (a ^ " " ^ b))

    and forbid_type f_ty node =
      match get_type node with
      | ty when compare f_ty ty = 0 -> raise (Type_error ("Operator pipe doesn't accept double"))
      | _ -> ()

    in match token with
       | Pipe ->
          forbid_type "double" left_node;
          forbid_type "double" right_node;
          match_type ()
       | _ -> match_type ()

  and infer = function
    (* binop *)
    | (Elem(Binop(token), l)) :: _ -> add_w_table (Hashtbl.find fct_name ""); infer_binop token l
    | (Elem(Type_binop(token, _), l)) :: t -> infer_binop token l @ t

    (* check for cond, and for step *)
    | (Elem(For_expr, l)) :: t ->
       let rec get_list scalar n = function
         | (Elem(Scalar(Less), l)) :: t when Less = scalar -> t
         | (Elem(Scalar(Comma), l)) :: t when Comma = scalar && n = 0 -> get_list scalar 1 t
         | (Elem(Scalar(Comma), l)) :: t when Comma = scalar && n = 1 -> t
         | (Elem(r, l)) :: t -> get_list scalar n t
         | _ -> []

       and check_coherence = function
         | ("double", "double") -> ()
         | ("double", b) -> raise (Type_error ("for: double " ^ b))
         | (a, "double") -> raise (Type_error ("for: " ^ a ^ " double"))
         | _ -> ()

       and check = function
         | (Elem(Scalar(For), l)) :: t ->
            let name = get_identifier t
            and ty = get_type (List.hd (get_expr Expression t))

            and cond = get_list Less 0 t in
            let c_ty = get_type (List.hd cond)

            and step = get_list Comma 0 t in
            let s_ty = get_type (List.hd step) in

            let _ = check_coherence (ty, c_ty)
            and _ = check_coherence (c_ty, s_ty)
            and _ = check_coherence (s_ty, ty)
            and _ = add_v_table (Hashtbl.find fct_name "") (name, ty)
            in (Elem(Scalar(For), l)) :: t

         | (Elem(r, l)) :: t -> (Elem(r, l)) :: check t
         | _ -> []
       in
       let v = check l
       in (Elem(For_expr, infer v )) :: infer t

    (* call *)
    | (Elem(Postfix, l)) :: t when List.length l > 1 ->
       infer_call t l
    | (Elem(r, l)) :: t ->
       let i = infer l
       in (Elem(r, i)) :: infer t
    | _ -> []
  in infer l
