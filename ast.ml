open Token
open Rule
open Debug

exception Syntax_error of string

let deb tokens = 
  debug_tokens tokens;
  (Leaf, tokens)

let error_check rule token = function
  | 1 ->
     print_string "Error at ";
     print_token token;
     raise (Syntax_error "")
  | _ -> ()

(* SCALAR *)
let assoc = function
  | Pow :: t -> Left
  | Time :: t -> Left
  | Div :: t -> Left
  | Mod :: t -> Left
  | Plus :: t -> Left
  | Minus :: t -> Left
  | More :: t -> Left
  | Less :: t -> Left
  | Pipe :: t -> Left
  | And :: t -> Left
  | tokens -> Rule.None

let binop = function
  | Pow :: t -> (Elem(Binop(Pow), []), t)
  | Time :: t -> (Elem(Binop(Time), []), t)
  | Div :: t -> (Elem(Binop(Div), []), t)
  | Mod :: t -> (Elem(Binop(Mod), []), t)
  | Plus :: t -> (Elem(Binop(Plus), []), t)
  | Minus :: t -> (Elem(Binop(Minus), []), t)
  | More :: t -> (Elem(Binop(More), []), t)
  | Less :: t -> (Elem(Binop(Less), []), t)
  | Pipe :: t -> (Elem(Binop(Pipe), []), t)
  | And :: t -> (Elem(Binop(And), []), t)
  | tokens -> (Leaf, tokens)

let unop = function
  | Plus :: t -> (Elem(Unop(Plus), []), t)
  | Minus :: t -> (Elem(Unop(Minus), []), t)
  | Lognot :: t -> (Elem(Unop(Lognot), []), t)
  | Non :: t -> (Elem(Unop(Non), []), t)
  | tokens -> (Leaf, tokens)

let scalar token = function
  | h :: t when h == token -> (Elem(Scalar(token), []), t)
  | tokens -> (Leaf, tokens)

let f_type = function
  | Name(v) :: t when v = "double" || v = "int" || v = "void" || v = "char" || v = "bool"
    -> (Elem(Type(Name(v)), []), t)
  | tokens -> (Leaf, tokens)

let prototype_intro = function
  | Name(v) :: t when v = "unary" || v = "binary"
    -> (Elem(Prototype_intro(Name(v)), []), t)
  | tokens -> (Leaf, tokens)

let double_const = function
  | Double(v) :: t -> (Elem(Double_const(Double(v)), []), t)
  | tokens -> (Leaf, tokens)

let integer_const = function
  | Integer(v) :: t -> (Elem(Integer_const(Integer(v)), []), t)
  | tokens -> (Leaf, tokens)

let char_const = function
  | Char(v) :: t -> (Elem(Char_const(Integer(v)), []), t)
  | tokens -> (Leaf, tokens)

let identifier = function
  | Name(v) :: t -> (Elem(Identifier(Name(v)), []), t)
  | tokens -> (Leaf, tokens)

(* AST PARSING *)
(* zero or multiple for simple pattern *)
let zero_max tokens rule op =
  let rec check node tokens op =
    match op tokens with
    | (Elem(Empty, l), t) -> check (l_node rule l node) t op
    | (Elem(r, l), t) -> check (a_node rule (Elem(r, l)) node) t op
    | _ -> (node, tokens)
  in check Pass tokens op

(* zero or one for simple pattern *)
let zero_or_one tokens rule op =
  match op tokens with
  | (Elem(Empty, l), t) -> (l_node rule l Leaf, t)
  | (Elem(r, l), t) -> (a_node rule (Elem(r, l)) Leaf, t)
  | _ -> (Pass, tokens)

(* sequence of rules that have to match *)
let sequence tokens rule list_op =
  let rec check s node tok = function
    | [] -> (node, tok)
    | op :: t_op ->
       match op tok with
       | (Elem(Empty, l), t) -> check 1 (l_node rule l node) t t_op
       | (Elem(r, l), t) -> check 1 (a_node rule (Elem(r, l)) node) t t_op
       | (Pass, t) -> check 1 node t t_op
       | _ -> error_check rule (List.hd tok) s; (Leaf, tokens)
  in check 0 Leaf tokens list_op

(* sequence of rules with first to match *)
let rec sequence_first tokens rule = function
  | [] -> (Leaf, tokens)
  | op :: t_op ->
     match op tokens with
     | (Elem(Empty, l), t) -> (l_node rule l Leaf, t)
     | (Elem(r, l), t) -> (a_node rule (Elem(r, l)) Leaf, t)
     | _ -> sequence_first tokens rule t_op

(* branching sequence on Left / Right  *)
let sequence_oriented tokens rule op op_l op_r =
  match assoc tokens with
    | Left -> sequence tokens rule [op; op_l]
    | Right -> sequence tokens rule [op; op_r]
    | _ -> (Leaf, tokens)
