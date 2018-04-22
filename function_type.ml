exception Undefined_variable of string
exception Undefined_function of string
exception Incoherent_signature of string

(* list of args types for by function *)
let f_table:(string, string list) Hashtbl.t = Hashtbl.create 10
(* list of tmp values types for by function *)
let v_table:(string, (string * string) list) Hashtbl.t = Hashtbl.create 10
let w_table:(string) Queue.t = Queue.create ()
let values:(string, string) Hashtbl.t = Hashtbl.create 10

let erase_fct name =
  Hashtbl.remove f_table name;
  Hashtbl.remove v_table name

let dump_f_table _ =
  let _ = print_endline "\nFunction table:" in
  let rec dump_ty = function
    | h :: t -> print_string (h ^ " "); dump_ty t
    | [] -> print_endline ""
  and dump_v = function
    | (v, ty) :: t -> print_string ("(" ^ v ^ " " ^ ty ^ ")"); dump_v t
    | [] -> print_endline ""
  and dump_v_table name =  print_string "Tmp val: "; dump_v (Hashtbl.find v_table name)
  in Hashtbl.iter (fun x y -> print_string (x ^ ": "); dump_ty y; dump_v_table x) f_table

let get_v_value fct name =
  let rec get_v = function
    | (v, ty) :: t when compare name v = 0 -> ty
    | h :: t -> get_v t
    | _ -> ""
  and l = Hashtbl.find v_table fct
  in get_v l

let add_f_table name args =
  let sign = List.map (fun (x, y) -> y) args
  and _ = Hashtbl.add v_table name []
  in Hashtbl.add f_table name sign

let add_w_table name =
  Queue.add name w_table

let add_v_table fct_name v =
  let rec check = function
    | ((va, ty), (s, sty) :: t) when compare va s != 0 -> (s, sty) :: check (v, t)
    | (v, (s, sty) :: t) ->
       let check_val =  function
         | ((va, ty), (s, sty) :: t) when compare ty sty = 0 -> (s, sty) :: t
         | ((va, ty), (s, "Unk") :: t) -> add_w_table fct_name; (s, ty) :: t
         | ((va, "Unk"), (s, sty) :: t) -> add_w_table fct_name; (s, sty) :: t
         | ((va, "char"), (s, "int") :: t) -> add_w_table fct_name; (s, "char") :: t
         | ((va, "bool"), (s, "int") :: t) -> add_w_table fct_name; (s, "char") :: t
         | ((va, "bool"), (s, "char") :: t) -> add_w_table fct_name; (s, "bool") :: t

         | ((va, "int"), (s, "char") :: t) -> (s, "char") :: t
         | ((va, "int"), (s, "bool") :: t) -> (s, "bool") :: t
         | ((va, "char"), (s, "bool") :: t) -> (s, "bool") :: t

         | ((va, ty), (s, sty) :: _) -> raise (Incoherent_signature (fct_name ^ ": " ^ ty ^ " " ^ sty))
         | _ -> raise (Incoherent_signature (fct_name ^ ":"))
       in check_val (v, (s, sty) :: t)

    | _ -> v :: []
  in
  let l = check (v, Hashtbl.find v_table fct_name)
  in Hashtbl.replace v_table fct_name l

let get_idtf_type fct_name v =
  try Hashtbl.find values v
  with
  | Not_found ->
     try
       let rec get_v = function
         | (vv, ty) :: t when compare v vv = 0 -> ty
         | (vv, ty) :: t -> get_v t
         | _ -> erase_fct fct_name; raise (Undefined_variable v)
       in get_v (Hashtbl.find v_table fct_name)
     with | Not_found ->  raise (Undefined_variable v)
                        
let get_w _ =
  Queue.pop w_table

let dump_w _ =
  print_endline "W_function:";
  Queue.iter (fun x -> print_endline x) w_table

let get_sign name =
  let sign = Hashtbl.find f_table name in
  List.rev (List.tl (List.rev sign))

let has_unk name =
  let sign = Hashtbl.find f_table name in
  List.exists (fun x -> compare x "Unk" = 0) sign

let modify_f_table name args =
  let rec check = function
    | ((v, ty) :: t, "Unk" :: ft) -> ty :: check (t, ft)
    | ((v, "Unk") :: t, f :: ft) -> f :: check (t, ft)
    | ((v, ty) :: t, f :: ft) when compare ty f = 0 -> ty :: check (t, ft)
(*
    | ((v, "char") :: t, "bool" :: ft) -> "bool" :: check (t, ft)
    | ((v, "int") :: t, "bool" :: ft) -> "bool" :: check (t, ft)
    | ((v, "bool") :: t, "char" :: ft) -> "bool" :: check (t, ft)
    | ((v, "bool") :: t, "int" :: ft) -> "bool" :: check (t, ft)
 *)
    | ((v, "int") :: t, "char" :: ft) -> "char" :: check (t, ft)
    | ((v, "char") :: t, "int" :: ft) -> "char" :: check (t, ft)

    | ((v, ty) :: t, f :: ft) -> raise (Incoherent_signature (name ^ ": " ^ ty ^ " " ^ f))
    | _ -> []
  in let f = check (args, (Hashtbl.find f_table name))
     in Hashtbl.replace f_table name f

let replace_signature name callee args_type =
  let rec check = function
    | ([], h :: t) -> raise (Incoherent_signature (name))
    | (h :: t, []) -> raise (Incoherent_signature (name))
    | ("Unk" :: t, f :: ft) -> f :: check (t, ft)

    | (ty :: t, "Unk" :: ft) -> add_w_table name; add_w_table callee; ty :: check (t, ft)

    (* down casting *)
    | ("bool" :: t, "int" :: ft) -> add_w_table name; add_w_table callee; "bool" :: check (t, ft)
    | ("bool" :: t, "char" :: ft) -> add_w_table name; add_w_table callee; "bool" :: check (t, ft)
    | ("char" :: t, "int" :: ft) -> add_w_table name; add_w_table callee; "char" :: check (t, ft)
    | ("int" :: t, "bool" :: ft) -> add_w_table name; add_w_table callee; "bool" :: check (t, ft)
    | ("int" :: t, "char" :: ft) -> add_w_table name; add_w_table callee; "char" :: check (t, ft)
    | ("char" :: t, "bool" :: ft) -> add_w_table name; add_w_table callee; "bool" :: check (t, ft)

    | (ty :: t, f :: ft) when compare ty f = 0 -> ty :: check (t, ft)
    | (ty :: t, f :: ft) -> raise (Incoherent_signature (name ^ ": " ^ ty ^ " " ^ f))
    | _ -> []

  and sign = try List.rev (Hashtbl.find f_table name) with | Not_found -> raise (Undefined_function name) in
  let ret = List.hd sign
  and f = check (args_type, List.rev (List.tl sign))
  in Hashtbl.replace f_table name (f @ [ret])

let replace_ret name callee ty =
  let rev_sign = List.rev (Hashtbl.find f_table name) in
  let ret = List.hd rev_sign
  and n_sign = List.rev (ty :: List.tl (rev_sign)) 
  in match (ret, ty) with
     | ("Unk", ty) -> Hashtbl.replace f_table name n_sign;
                      add_w_table name;
                      add_w_table callee
     | (ret, ty) when compare ret ty = 0 -> ()
     | (ret, ty) -> raise (Incoherent_signature (name ^ " " ^ ret ^ " " ^ ty ))

let get_ret name =
  try 
    List.hd (List.rev (Hashtbl.find f_table name))
  with Not_found -> raise (Undefined_function name)
