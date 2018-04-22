open Token
open Rule

let print_token = function
  | Eof -> print_endline "Eof";
  | Def -> print_endline "Def";
  | Extern -> print_endline "Extern";
  | Assign -> print_endline "Assign";
  | Plus -> print_endline "Plus";
  | Minus -> print_endline "Minus";
  | Time -> print_endline "Time";
  | Div -> print_endline "Division";
  | Mod -> print_endline "Modulo";
  | Open -> print_endline "Open";
  | Close -> print_endline "Close";
  | End -> print_endline "End";
  | Equal -> print_endline "Equal";
  | Less -> print_endline "Less";
  | More -> print_endline "More";
  | Lognot -> print_endline "Lognot";
  | Non -> print_endline "Non";
  | While -> print_endline "While";
  | Do -> print_endline "Do";
  | If -> print_endline "If";
  | Then -> print_endline "Then";
  | Else -> print_endline "Else";
  | For -> print_endline "For";
  | In -> print_endline "In";
  | Comma -> print_endline "Comma";
  | Pow -> print_endline "Power";
  | Pipe -> print_endline "Pipe";
  | And -> print_endline "And";
  | Wildcard -> print_endline "Wildcard";
  | Quote -> print_endline "Quote";
  | Doublequote -> print_endline "Doublequote";
  | Name(v) -> print_endline ( "Name " ^ v );
  | Double(v) -> print_endline ( "Double " ^ (string_of_float v) );
  | Integer(v) -> print_endline ( "Integer " ^ (string_of_int v) );
  | Char(v) -> print_endline ( "Char " ^ (string_of_int v) );
;;

let rec debug_tokens = function
  | h :: t -> print_token h; debug_tokens t
  | _ -> ()

let print_rule c rule =
  Printf.printf "%0*s%d %s" c "" c rule 

let print_expr c = function
  | Unop(token) -> print_rule c "Unop "; print_token token
  | Binop(token) -> print_rule c "Binop "; print_token token
  | Scalar(token) -> print_rule c "Scalar "; print_token token

  | Double_const(token) -> print_rule c "Double_const "; print_token token
  | Integer_const(token) -> print_rule c "Integer_const "; print_token token
  | Char_const(token) -> print_rule c "Char_const "; print_token token
  | Bool_const(token) -> print_rule c "Bool_const "; print_token token

  | Identifier(token) -> print_rule c "Identifier "; print_token token
  | Type(token) -> print_rule c "Type "; print_token token
  | Prototype_intro(token) -> print_rule c "Prototype_intro "; print_token token

  | Arg(v, t) -> print_rule c "Arg ";  print_endline (v ^ " " ^ t)
  | Type_binop(token, t) -> print_rule c "Type_binop ";  print_string (t ^ " "); print_token token
  | Unary_call(idtf, ty) -> print_rule c "Unary_call ";  print_string (idtf ^ " " ^ ty)

  | Literal -> print_rule c "Literal\n"

  | Top -> print_rule c "Top\n"
  | Stmt -> print_rule c "Stmt\n"
  | Kdefs -> print_rule c "Kdefs\n"
  | Prototype -> print_rule c "Prototype\n"
  | Prototype_args -> print_rule c "Prototype_args\n"
  | Expressions -> print_rule c "Expressions\n"
  | Expression -> print_rule c "Expression\n"

  | For_expr -> print_rule c "For_expr\n"
  | If_expr -> print_rule c "If_expr\n"
  | While_expr -> print_rule c "While_expr\n"
  | Call_expr -> print_rule c "Call_expr\n"
  | Paren_expr -> print_rule c "Paren_expr\n"

  | Top_expr -> print_rule c "Top_expr\n"
  | Ext_def -> print_rule c "Ext_def\n"
  | Local_def -> print_rule c "Local_def\n"

  | Primary -> print_rule c "Primary\n"
  | Unary -> print_rule c "Unary\n"
  | Postfix -> print_rule c "Postfix\n"
  | Defs -> print_rule c "Defs\n"


  | Empty -> print_rule c "Empty\n"

let debug_ast node =
  let rec count c = function 
    | (Elem(r, l)) :: t ->
       print_expr c r;
       count (c + 1) l;
       count c t;
    | _ -> ()
  in print_endline "Ast :"; count 0 node
