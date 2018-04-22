open Llvm
open Llvm_executionengine
open Ctypes
open Foreign
open Str
open Rule
open Token
open Misc_type
open Function_type

exception Unknown_type of string
exception Already_defined of string

let add_target_triple triple llm =
  Llvm_X86.initialize ();
  let lltarget = Llvm_target.Target.by_triple triple in
  let llmachine = Llvm_target.TargetMachine.create ~triple:triple lltarget in
  let lldly = Llvm_target.TargetMachine.data_layout llmachine in

  set_target_triple (Llvm_target.TargetMachine.triple llmachine) llm;
  set_data_layout (Llvm_target.DataLayout.as_string lldly) llm;
  ()

let llvm_context = global_context ()
let llvm_module = create_module llvm_context "test"
let _ = add_target_triple "x86_64" llvm_module
let llvm_builder = builder llvm_context
let ll_values:(string, lltype) Hashtbl.t = Hashtbl.create 10
let named_values:(string, llvalue) Hashtbl.t = Hashtbl.create 10
let named_functions:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let stored_functions:(string, llvalue) Hashtbl.t = Hashtbl.create 10

let anonymous = ref(0)



(* misc *)
let llvm_type = function
  | "double" -> double_type llvm_context
  | "int" -> i32_type llvm_context
  | "char" -> i8_type llvm_context
  | "bool" -> i8_type llvm_context
  | "void" -> void_type llvm_context
  | e -> raise (Unknown_type ("ll " ^ e))

let get_cmp = function
  | "double" -> build_fcmp Fcmp.Ult
  | "int" -> build_icmp Icmp.Ult
  | "char" -> build_icmp Icmp.Ult
  | "bool" -> build_icmp Icmp.Ult
  | e -> raise (Unknown_type ("cmp " ^ e))

let get_bool_cond = function
  | "double" -> const_float (double_type llvm_context) 0.
  | "int" -> const_int (i32_type llvm_context) 0
  | "char" -> const_int (i8_type llvm_context) 0
  | "bool" -> const_int (i8_type llvm_context) 0
  | e -> raise (Unknown_type ("bool " ^ e))

let get_bool_and = function
  | "double" -> const_float (double_type llvm_context) 1.
  | "int" -> const_int (i32_type llvm_context) 1
  | "char" -> const_int (i8_type llvm_context) 1
  | "bool" -> const_int (i8_type llvm_context) 1
  | e -> raise (Unknown_type ("bool " ^ e))

let type_add = function
  | "double" -> build_fadd
  | "int" -> build_add
  | "char" -> build_add
  | "bool" -> build_add
  | e -> raise (Unknown_type ("add " ^ e))

let rec fill_table = function
  | (Elem(Arg(v, ty), _)) :: t ->
     Hashtbl.add ll_values v (llvm_type ty);
     Hashtbl.add values v ty;
     fill_table t
  | _ -> ()

let rec get_arg_names = function
  | (Elem(Arg("", _), _)) :: t -> []
  | (Elem(Arg(v, ty), _)) :: t -> v :: get_arg_names t
  | h :: t -> get_arg_names t
  | _ -> []

let rec get_arg_types = function
  | (Elem(Arg("", _), _)) :: t -> []
  | (Elem(Arg(v, ty), _)) :: t -> llvm_type ty :: get_arg_types t
  | h :: t -> get_arg_types t
  | _ -> []

(* build asm for function declaration *)
let asm_proto l =
  let _ = fill_table (get_expr Prototype_args l)
  and idtf = get_identifier l in
  let _ = Hashtbl.replace fct_name "" idtf

  and names = get_arg_names (get_expr Prototype_args l)
  and types = get_arg_types (get_expr Prototype_args l) in

  let ftype = function_type (Hashtbl.find ll_values "") (Array.of_list types) in
  let fct_decl = declare_function idtf ftype llvm_module in
  let _ = Hashtbl.add named_functions idtf fct_decl
  and set_table name value = set_value_name name value; Hashtbl.add named_values name value in
  let _ = List.map2 (fun x y -> set_table x y) names (Array.to_list (params fct_decl))
  in fct_decl

(* start asm for function implementation *)
let asm_defs prototype =
  let _ = Hashtbl.clear ll_values
  and _ = Hashtbl.clear values
  and proto = asm_proto prototype in
  let body = append_block llvm_context "entry" proto in
  position_at_end body llvm_builder;
  proto

(* build asm for an expr *)
let rec asm_expr = function

  | (Elem(If_expr, l)) ->
     let rec if_sequ fct incoming bb bb_ = function
       | (Elem(Scalar(Then), l)) :: t ->

          (* Emit 'then' value. *)
          let then_bb = append_block llvm_context "then" fct in
          position_at_end then_bb llvm_builder;
          let then_val = asm_expr (List.hd t)
          and null = const_int (i8_type llvm_context) 0
          and ty = get_type (List.hd t) in
          let ret = if compare ty "void" = 0 then null else then_val
          and new_then_bb = insertion_block llvm_builder
          in if_sequ fct (incoming @ [(ret, new_then_bb)]) (bb @ [then_bb]) (bb_ @ [new_then_bb]) t

       | (Elem(Scalar(Else), l)) :: t ->
          (* Emit 'else' value. *)
          let else_bb = append_block llvm_context "else" fct in
          position_at_end else_bb llvm_builder;
          let else_val = asm_expr (List.hd t)
          and null = const_int (i8_type llvm_context) 0
          and ty = get_type (List.hd t) in
          let ret = if compare ty "void" = 0 then null else else_val
          and new_else_bb = insertion_block llvm_builder
          in if_sequ fct (incoming @ [(ret, new_else_bb)]) (bb @ [else_bb]) (bb_ @ [new_else_bb]) t 

       | (Elem(r, l)) :: t -> if_sequ fct incoming bb bb_ t
       | _ -> (incoming, bb, bb_)

     and build_if = function
       | (Elem(Scalar(If), l)) :: t ->
          (* condition *)
          let cond = asm_expr (List.hd t)
          and ty = get_type (List.hd t) in
          let zero = get_bool_cond ty in
          let cond_val = (get_cmp ty) zero cond "ifcond" llvm_builder in

          (* define block *)
          let start_bb = insertion_block llvm_builder in
          let fct = block_parent start_bb in

          (* get then else sequence blocks *)
          let values = if_sequ fct [] [] [] t in
          let incoming = match values with | (i,_,_) -> i
          and bb1 = match values with | (_,b,_) -> List.nth b 0
          and bb2 = match values with | (_,b,_) -> List.nth b 1
          and nbb1 = match values with | (_,_,n) -> List.nth n 0
          and nbb2 = match values with | (_,_,n) -> List.nth n 1 in

          (* link blocks *)
          let merge_bb = append_block llvm_context "ifcont" fct in
          position_at_end merge_bb llvm_builder;
          let phi = build_phi incoming "phi" llvm_builder
          in position_at_end start_bb llvm_builder;
             ignore (build_cond_br cond_val bb1 bb2 llvm_builder);
             position_at_end nbb1 llvm_builder; ignore (build_br merge_bb llvm_builder);
             position_at_end nbb2 llvm_builder; ignore (build_br merge_bb llvm_builder);
             position_at_end merge_bb llvm_builder;
             phi

       | (Elem(r, l)) :: t -> build_if t
       | _ -> raise (Unknown_type "if")
     in build_if l
      
  | (Elem(For_expr, list)) ->
     
     let rec get_list scalar n = function
       | (Elem(Scalar(For), l)) :: t when scalar = For -> t
       | (Elem(Scalar(Comma), l)) :: t when scalar = Comma && n = 0-> get_list Comma 1 t
       | (Elem(Scalar(Comma), l)) :: t when scalar = Comma && n = 1 -> t
       | (Elem(Scalar(In), l)) :: t when scalar = In -> t
       | (Elem(Scalar(Less), l)) :: t when scalar = Less -> t
       | (Elem(r, l)) :: t -> get_list scalar n t
       | _ -> []
     in

     (* init variable *)
     let l_for = get_list For 0 list in 
     let var_name = get_identifier l_for in
     let start_val = asm_expr (List.hd (get_expr Expression l_for))
     and v_ty = get_v_value (Hashtbl.find fct_name "") var_name in
     let cast_val = build_intcast start_val (llvm_type v_ty) "tmpcasttmp" llvm_builder in

     (* start block *)
     let preheader_bb = insertion_block llvm_builder in
     let fct = block_parent preheader_bb in
     let loop_bb = append_block llvm_context "loop" fct in
     ignore (build_br loop_bb llvm_builder);
     position_at_end loop_bb llvm_builder;

     (* start Phi block *)     
     let variable = build_phi [(cast_val, preheader_bb)] var_name llvm_builder
     and l_in = get_list In 0 list  in
     Hashtbl.add named_values var_name variable;
     ignore (asm_expr (List.hd l_in));

     (* add variable *)
     let l_step = get_list Comma 0 list in
     let step_val = asm_expr (List.hd l_step) in
     let ty_step = get_type (List.hd l_step) in
     let cast_add = build_intcast step_val (llvm_type v_ty) "castaddtmp" llvm_builder in
     let next_var = (type_add ty_step) variable cast_add "nextvar" llvm_builder in

     (* cond variable *)
     let l_end = get_list Less 0 list in
     let end_val = asm_expr (List.hd l_end)
     and end_ty = get_type (List.hd l_end) in
     let cast_cmp = build_intcast end_val (llvm_type v_ty) "castcmptmp" llvm_builder in
     let cmp_val = (get_cmp end_ty) next_var cast_cmp "cmptmp" llvm_builder in
     let castbooltmp = build_intcast cmp_val (i8_type llvm_context) "booltmp" llvm_builder in
     let end_cond = build_icmp Icmp.Ult (get_bool_cond "char") castbooltmp  "loopcond" llvm_builder in

     (* Phi block *)
     let loop_end_bb = insertion_block llvm_builder in
     let after_bb = append_block llvm_context "afterloop" fct in
     ignore (build_cond_br end_cond loop_bb after_bb llvm_builder);
     position_at_end after_bb llvm_builder;
     add_incoming (next_var,loop_end_bb) variable;

     (*  *)
     if compare (Hashtbl.find values "") "void" = 0
     then const_null (i32_type llvm_context)
     else const_null (Hashtbl.find ll_values "")

  | (Elem(Expressions, l)) when List.length l > 1 ->
     let rec check = function
       | (Elem(Expression, l)) :: t when t = [] -> asm_expr (List.hd l)
       | (Elem(Expression, l)) :: t -> let _ = asm_expr (List.hd l) in check t
       | (Elem(r, l)) :: t -> check t
       | _ -> const_int (i8_type llvm_context) 0
     in check l     

  | (Elem(Unary, l)) ->
     let rec asm_call idtf = function
       | (Elem(r, l)) :: t ->
          let args = Array.of_list (get_args [] l) in
          let fct = Hashtbl.find named_functions idtf
          and ret = if compare (get_ret idtf) "void" = 0 then "" else "ret_call" in
          let f = build_call fct args ret llvm_builder
          in f
       | _ -> raise (Unknown_type "call")

     and asm_prim = function
       | (Elem(Primary, l)) :: t when List.length t > 0 -> asm_call (get_identifier l) t
       | (Elem(Double_const(Double(v)), _)) :: t -> const_float (double_type llvm_context) v
       | (Elem(Integer_const(Integer(v)), _)) :: t -> const_int (i32_type llvm_context) v
       | (Elem(Char_const(Integer(v)), _)) :: t -> const_int (i8_type llvm_context) v
       | (Elem(Bool_const(Integer(v)), _)) :: t ->
          let b = if v = 0 then 0 else 1
          in const_int (i8_type llvm_context) b
       | (Elem(Identifier(Name(v)), _)) :: t -> Hashtbl.find named_values v

       | (Elem(Paren_expr, l)) :: t -> asm_expr (List.hd (get_scalar Open l))

       | (Elem(_, l)) :: t -> asm_prim l
       | _ -> raise (Unknown_type "prim")

     and put_unop v = function
       | (Minus, _) -> const_nuw_neg v
       | (Plus, _) -> v
       | (Lognot, "double") ->
          let zero = get_bool_cond "double" in
          let bool = (get_cmp "double") zero v "booltmp" llvm_builder
          in build_sitofp bool (llvm_type "double") "casttmp" llvm_builder

       | (Lognot, ty) ->
          let zero = get_bool_cond ty in
          let bool = (get_cmp ty) zero v "booltmp" llvm_builder
          in build_intcast bool (llvm_type ty) "casttmp" llvm_builder
               
       | (Non, "double") -> raise (Type_error "Bitwise incompatible with double")
       | (Non, _) -> const_not v

       | _ -> v

     and add_unop ty v = function
       | (Elem(Unop(token), l)) :: t -> add_unop ty (put_unop v (token, ty)) t
       | (Elem(r, l)) :: t -> add_unop ty v t
       | _ -> v

     in let v = asm_prim l
        in add_unop (get_type (List.hd l)) v l

  | (Elem(Type_binop(token, ty), l)) ->
     let cast cast_op v ty =
       cast_op v (llvm_type ty) "casttmp" llvm_builder

     (* more / less *)
     and cmp op op_type l r cast =
       let i = op op_type l r "cmptmp" llvm_builder
       in cast i (i8_type llvm_context) "booltmp" llvm_builder

     (* pipe *)
     and op_or ty l r =
       let i = build_or l r "ortmp" llvm_builder
       and zero = get_bool_cond ty in
       let bool = (get_cmp ty) zero i "booltmp" llvm_builder
       in build_intcast bool (i8_type llvm_context) "boolcasttmp" llvm_builder

     (* and *)
     and op_and ty l r =
       let i = build_and l r "andtmp" llvm_builder
       and zero = get_bool_cond ty in
       let bool = (get_cmp ty) zero i "booltmp" llvm_builder
       in build_intcast bool (i8_type llvm_context) "boolcasttmp" llvm_builder

     and pow ty l r =
       let d = (llvm_type "double") in
       let ftype = function_type d (Array.of_list [d;d;]) in
       let fct_decl = declare_function "pow" ftype llvm_module in
       let p =  build_call fct_decl (Array.of_list [l;r;]) "pow_ret" llvm_builder in
       if compare ty "double" = 0 then p
       else build_fptosi p (llvm_type "int") "tmp2" llvm_builder

     and build_op op l r name = op l r name llvm_builder in
     let op l r = function
         
       | (Plus, "double") -> build_op build_fadd l r "addtmp"
       | (Plus, "int") -> build_op build_add l r "addtmp"
       | (Plus, "char") -> build_op build_add l r "addtmp"
       | (Plus, "bool") -> build_op build_add l r "addtmp"

       | (Minus, "double") -> build_op build_fsub l r "subtmp"
       | (Minus, "int") -> build_op build_sub l r "subtmp"
       | (Minus, "char") -> build_op build_sub l r "subtmp"
       | (Minus, "bool") -> build_op build_sub l r "subtmp"

       | (Time, "double") -> build_op build_fmul l r "multmp"
       | (Time, "int") -> build_op build_mul l r "multmp"
       | (Time, "char") -> build_op build_mul l r "multmp"
       | (Time, "bool") -> build_op build_mul l r "multmp"

       | (Div, "double") -> build_op build_fdiv l r "multmp"
       | (Div, "int") -> build_op build_sdiv l r "multmp"
       | (Div, "char") -> build_op build_sdiv l r "multmp"
       | (Div, "bool") -> build_op build_sdiv l r "multmp"

       | (Mod, "double") -> build_op build_frem l r "multmp"
       | (Mod, "int") -> build_op build_srem l r "multmp"
       | (Mod, "char") -> build_op build_srem l r "multmp"
       | (Mod, "bool") -> build_op build_srem l r "multmp"

       | (More, "bool") -> cmp build_icmp Icmp.Ult r l build_intcast
       | (More, "char") -> cmp build_icmp Icmp.Ult r l build_intcast
       | (More, "int") -> cmp build_icmp Icmp.Ult r l build_intcast
       | (More, "double") -> cmp build_fcmp Fcmp.Ult r l build_intcast

       | (Less, "bool") -> cmp build_icmp Icmp.Ult r l build_intcast
       | (Less, "char") -> cmp build_icmp Icmp.Ult r l build_intcast
       | (Less, "int") -> cmp build_icmp Icmp.Ult r l build_intcast
       | (Less, "double") -> cmp build_fcmp Fcmp.Ult r l build_intcast

       | (Pow, "bool") ->
          pow "bool" (cast build_sitofp l "double") (cast build_sitofp r "double")
       | (Pow, "char") ->
          pow "bool" (cast build_sitofp l "double") (cast build_sitofp r "double")
       | (Pow, "int") ->
          pow "bool" (cast build_sitofp l "double") (cast build_sitofp r "double")
       | (Pow, "double") -> pow "double" l r

       | (And, ty) -> op_and ty l r

       | (Pipe, ty) -> op_or ty l r

       | (_, ty) -> raise (Unknown_type ("type :" ^ ty))

     and left_val = asm_expr (List.nth l 0)
     and right_val = asm_expr (List.nth l 1)
     in op left_val right_val (token, ty)
  | (Elem(r, l)) -> asm_expr (List.hd l)
  | _ -> raise (Unknown_type "op")

and get_args acc = function 
  | (Elem(Expression, l)) :: t -> get_args (asm_expr (List.hd l) :: acc) t
  | h :: t -> get_args acc t
  | _ -> List.rev acc

(* build asm for top_expr *)
let asm_main l =
  let fty = function_type (i32_type llvm_context) [| |] in
  let main = declare_function "main" fty llvm_module in
  let body = append_block llvm_context "entry" main in
  let _ = position_at_end body llvm_builder
  and _ = asm_expr (List.hd (get_expr Expressions l)) 
  and ret = const_int (i32_type llvm_context) 0 in
  let _ = build_ret ret llvm_builder;
  in Llvm_analysis.assert_valid_function main;
     main

(* build asm for functions *)
let rec asm_functions prompt = function

  (* include *)
  | (Elem(Ext_def, l)) :: t -> 
     let fct = asm_proto (get_expr Prototype l)
     in dump_value fct;
        asm_functions prompt l;
        asm_functions prompt t
        
  (* local function *)
  | (Elem(Defs, l)) :: t ->
     let name = get_identifier (get_expr Prototype l) in
     let _ = try let _ = Hashtbl.find stored_functions name 
                 in raise (Already_defined name)
             with | Not_found -> ()
     and fct = asm_defs (get_expr Prototype l)
     and ret = asm_expr (List.hd (get_expr Expressions l)) in
     let _ = if compare (get_ret name) "void" = 0
             then build_ret_void llvm_builder
             else build_ret ret llvm_builder
     in Llvm_analysis.assert_valid_function fct;
        Hashtbl.add stored_functions name fct;
        dump_value fct;        
        asm_functions prompt l;
        asm_functions prompt t

  (* main *)
  | (Elem(Top_expr, l)) :: t ->
     begin
       match prompt with
       | 0 -> 
          let name = "anonym_" ^ (string_of_int !anonymous)
          and _ = anonymous:= !anonymous + 1
          and ty = get_type (List.hd (get_expr Expressions l))
          and exec_anonym name engine = function
            | "double" -> let i = get_function_address name (funptr (void @-> returning double)) engine
                          in print_float (i ())
            | "int" -> let i = get_function_address name (funptr (void @-> returning int)) engine
                       in print_int (i ())
            | "char" -> let i = get_function_address name (funptr (void @-> returning char)) engine
                        in print_char (i ())
            | "bool" -> let i = get_function_address name (funptr (void @-> returning bool)) engine
                        in print_string (string_of_bool (i ()))
            | "void" -> let i = get_function_address name (funptr (void @-> returning void)) engine
                        in (i ())
            | _ -> raise (Type_error "")
          and build_anonym ty name =
            let fty = function_type (llvm_type ty) [||] in
            let fct = declare_function name fty llvm_module in
            let body = append_block llvm_context "entry" fct in
            let _ = position_at_end body llvm_builder
            and ret = asm_expr (List.hd (get_expr Expressions l)) in
            let _ = if compare ty "void" = 0
                    then build_ret_void llvm_builder
                    else build_ret ret llvm_builder
            and _ = Llvm_analysis.assert_valid_function fct
            in fct
          in let fct = build_anonym ty name
             and engine = create llvm_module in
             let _ = dump_value fct in
             exec_anonym name engine ty;
             print_endline ""
       | _ -> let main = asm_main l
              in dump_value main;
                 asm_functions prompt l;
                 asm_functions prompt t
     end
  | (Elem(r, l)) :: t ->
     asm_functions prompt l;
     asm_functions prompt t
  | _ -> ()

let build_asm ast filename =
  ignore (initialize ());
  let _ = asm_functions (compare filename "") ast
  in match filename with
     | "" -> ()
     | name ->
        let file = List.hd (Str.split (regexp "\\.") name) in
        Llvm_analysis.assert_valid_module llvm_module;
        let _ = Llvm_bitwriter.write_bitcode_file llvm_module (file ^ ".bc")
        and _  = Sys.command ("llc-5.0 " ^ file ^ ".bc")
        and _ = Sys.command ("clang -o a.out " ^ file ^ ".s -lm")
        in ()
;;

(* si prompt, pas de main *)
(* sauvegarder les ast *)

(* si prompt, obliger les types *)
(* jit -> supprime le main, pas de main *)
