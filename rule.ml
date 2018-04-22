open Token

type rule =
  (* scalar *)
  | Binop of token
  | Unop of token
  | Scalar of token
  | Identifier of token
  | Double_const of token
  | Integer_const of token
  | Char_const of token
  | Bool_const of token
  | Type of token
  | Prototype_intro of token

  (* abstract rules*)
  | Top
  | Stmt
  | Kdefs
  | Prototype
  | Prototype_args
  | Expression
  | Expressions

  | For_expr
  | If_expr
  | While_expr
  | Call_expr
  | Paren_expr

  | Top_expr
  | Ext_def
  | Local_def

  | Primary
  | Literal
  | Unary
  | Postfix
  | Defs

  | Empty

  (* arg : identifier * type *)
  | Arg of string * string

  (* type_binop : token * type *)
  | Type_binop of token * string

  (* type_binop : name * type *)
  | Unary_call of string * string

type node =
  | Elem of rule * node list
  | Leaf
  | Pass

type associative =
  | Left
  | Right
  | None

let l_p_node child = function
  | Elem(r, l) -> Elem(r, [child] @ l )
  | _ -> Elem(Empty, [child])

let p_node child = function
  | Elem(r, l) -> Elem(r, l @ [child] )
  | _ -> Elem(Empty, [child])

let a_node r child = function
  | Elem(_, l) -> Elem(r, l @ [child] )
  | _ -> Elem(r, [child])

let l_node r children = function
  | Elem(_, l) -> Elem(r, l @ children )
  | _ -> Elem(r, children)

