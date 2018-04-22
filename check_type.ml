open Rule
open Function_type
open Arg_type
open Misc_type
open Infer_type

(* what defines an implicit type *)
(* equivalence in a binop => a + 3, a + 3. *) 
(* value assignation, in call_expr (fun(a) a; => fun(3) ) *)

(* INFER TYPE *)
let check_type l =
  (* remove undefined functions *)
  let rec remove_dead_code ast =
    let check r l =
      let name = get_identifier (get_expr Prototype l)
      in match has_unk name with
         | true -> erase_fct name; print_endline ("Function " ^ name ^ " misses type info"); []
         | false -> (Elem(r, l)) :: []
    in match ast with
       | (Elem(Ext_def, l)) :: t -> check Ext_def l
       | (Elem(Local_def, l)) :: t -> check Local_def l
       | (Elem(Top_expr, l)) :: t -> (Elem(Top_expr, l)) :: []
       | (Elem(r, l)) :: t -> (Elem(r, remove_dead_code l)) :: remove_dead_code t
       | _ -> []

  (* infer op types, call_types, return type, modify f_table, and arg declaration *)
  and run name ast =
    let check r l =
      let name = (get_identifier (get_expr Prototype l)) in
      let _ = act_table name l
      and i = infer_type l in
      let b = ret_type i  in
      let p = change_proto b in
      let _ = modify_f_table name (get_args p)
      in (Elem(r, p)) :: []

    and cmp l = compare (get_identifier (get_expr Prototype l)) name = 0
    in match ast with
    | (Elem(Ext_def, l)) :: t when cmp l -> check Ext_def l
    | (Elem(Defs, l)) :: t when cmp l -> check Defs l
    | (Elem(Top_expr, l)) :: t ->
       Hashtbl.replace fct_name "" "";
       let i = infer_type l
       in (Elem(Top_expr, i)) :: []
        
    | (Elem(r, l)) :: t ->
       let i = run name l 
       in (Elem(r, i)) :: run name t

    | _ -> []

  (* rebuild proto and add elem in f_table *)
  and first_run = function
    | (Elem(Prototype, l)) :: t ->
       
       let name = get_identifier l
       and i = change_args l in
       let args = get_args i in
       let _ = add_f_table name args
       and _ = add_w_table name
       in (Elem(Prototype, i)) :: t

    | (Elem(r, l)) :: t ->
       let i = first_run l
       in (Elem(r, i)) :: first_run t
    | _ -> []

  (* main function *)
  in let ast = first_run l in
     let rec r = run "" ast
     and converge ast =
       try 
         let name = (get_w ()) in 
         let l = run name ast
         in converge l
       with | Queue.Empty -> remove_dead_code ast
     in converge r
;;
