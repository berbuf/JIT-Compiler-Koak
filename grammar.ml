open Token
open Rule
open Ast

(* expressions  <- for_expr / if_expr / while_expr / expression (':' expression)* *)
let rec expressions tokens =
  let rec sc tok = scalar Assign tok
  and seq_2 tok = sequence tok Empty [sc; expression]
  and zer tok = zero_max tok Empty seq_2 
  and seq_1 tok = sequence tok Empty [expression; zer]
  in sequence_first tokens Expressions [for_expr; if_expr; seq_1]

(* while_expr  <- 'while ' expression  'do' expressions *)
and while_expr tokens =
  let sc_1 tok = scalar While tok
  and sc_2 tok = scalar Do tok
  in sequence tokens While_expr [sc_1; expression; sc_2; expressions]

(* if_expr <- 'if ' expression "then" expressions ("else" expressions)? *)
and if_expr tokens =
  let rec sc_1 tok = scalar If tok
  and sc_2 tok = scalar Then tok
  and sc_3 tok = scalar Else tok
  and seq tok = sequence tok Empty [sc_3; expressions]
  and opt tok = zero_or_one tok Empty seq
  in sequence tokens If_expr [sc_1; expression; sc_2; expressions; opt]

(* for_expr  <- 'for ' identifier  '=' expression ',' identifier  '<' expression ',' expression  'in ' expressions *)
and for_expr tokens =
  let rec sc_1 tok = scalar For tok
  and sc_2 tok = scalar Equal tok
  and sc_3 tok = scalar Comma tok
  and sc_4 tok = scalar Less tok
  and sc_5 tok = scalar In tok
  and l_op = [sc_1; identifier; sc_2; expression; sc_3; identifier; sc_4;
              expression; sc_3; expression; sc_5; expressions]
  in sequence tokens For_expr l_op

(* expression <- unary (#binop (#left_assoc unary / #right_assoc expression))* *)
and expression tokens =
  let rec seq_or tok = sequence_oriented tok Empty binop unary expression
  and one_z tok = zero_max tok Empty seq_or
  in sequence tokens Expression [unary; one_z]

(* unary <- (#unop unary) / postfix *)
and unary tokens =
  let seq tok = sequence tok Empty [unop; unary]
  in sequence_first tokens Unary [seq; postfix]

(* postfix <- primary call_expr? *)
and postfix tokens =
  let opt tok = zero_or_one tok Empty call_expr
  in sequence tokens Postfix [primary; opt] 

(* primary  <- identifier / literal / paren_expr *)
and primary tokens =
  sequence_first tokens Primary [identifier; literal; paren_expr]

(* literal  <- double_const / integer const / char_const *)
and literal tokens =
  sequence_first tokens Literal [double_const; integer_const; char_const]

(* call_expr <- '(' (expression (',' expression)* ) ? ')' *)
and call_expr tokens =
  let rec sc_1 tok = scalar Open tok
  and sc_2 tok = scalar Close tok
  and sc_3 tok = scalar Comma tok
  and seq_1 tok = sequence tok Empty [sc_3; expression]
  and zer tok = zero_max tok Empty seq_1
  and seq_2 tok = sequence tok Empty [expression; zer]
  and opt tok = zero_or_one tok Empty seq_2
  in sequence tokens Call_expr [sc_1; opt; sc_2]

(* paren_expr <- '(' expressions ') *)
and paren_expr tokens =
  let sc_1 tok = scalar Open tok
  and sc_2 tok = scalar Close tok
  in sequence tokens Paren_expr [sc_1; expressions; sc_2;]

(* prototype_args <- '(' (identifier ( ':' type)? )* ')' (':' type)? *)
let prototype_args tokens =
  let rec sc_1 tok = scalar Open tok
  and sc_2 tok = scalar Close tok
  and sc_3 tok = scalar Assign tok
  and seq_1 tok = sequence tok Empty [sc_3; f_type]
  and opt tok = zero_or_one tok Empty seq_1
  and seq_2 tok = sequence tok Empty [identifier; opt]
  and zer tok = zero_max tok Empty seq_2
  in sequence tokens Prototype_args [sc_1; zer; sc_2; opt;]

let decimal_const tokens =
  (Leaf, tokens)

(* prototype  <- ('unary ' . decimal_const? / 'binary ' . decimal_const? / identifier) prototype_args *)
let prototype tokens =
  let rec opt tok = zero_or_one tok Empty decimal_const
  and seq_1 tok = sequence tokens Empty [prototype_intro; opt] 
  and seq_f tok = sequence_first tok Empty [seq_1; identifier]
  in sequence tokens Prototype [seq_f; prototype_args]

(* defs <- prototype expressions *)
let defs tokens =
  sequence tokens Defs [prototype; expressions]

(* ext_def <- "extern" protptype ';' *)
let ext_def tokens =
  let sc_1 tok = scalar Extern tok
  and sc_2 tok = scalar End tok
  in sequence tokens Ext_def [sc_1; prototype; sc_2]

(* local_def <- "def" defs ';' *)
let local_def tokens =
  let sc_1 tok = scalar Def tok
  and sc_2 tok = scalar End tok
  in sequence tokens Local_def [sc_1; defs; sc_2]

(* top_expr <- expressions ';' *)
let top_expr tokens =
  let sc tok = scalar End tok
  in sequence tokens Top_expr [expressions; sc]

(* kdefs  <- ext_def / local_def / top_expr *)
let kdefs tokens =
  sequence_first tokens Kdefs [ext_def; local_def; top_expr]

(* stmt <- kdefs* #eof *)
let stmt tokens =
  let zer tok = zero_max tok Empty kdefs
  and sc tok = scalar Eof tok
  in sequence tokens Stmt [zer; sc]

let ast tokens =
  let root = Elem(Top, [])
  in match stmt tokens with
     | (Elem(r, l), t)-> (a_node Top (Elem(r, l)) root)
     | _ -> root 
