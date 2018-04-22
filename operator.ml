open Token
open Rule

let prevalence = function
  | (Elem(Binop(Equal), _)) -> 10
  | (Elem(Binop(Pipe), _)) -> 40
  | (Elem(Binop(And), _)) -> 60
  | (Elem(Binop(Less), _)) -> 80
  | (Elem(Binop(More), _)) -> 80
  | (Elem(Binop(Plus), _)) -> 100
  | (Elem(Binop(Minus), _)) -> 100
  | (Elem(Binop(Time), _)) -> 110
  | (Elem(Binop(Div), _)) -> 110
  | (Elem(Binop(Mod), _)) -> 110
  | (Elem(Binop(Pow), _)) -> 120
  | _ -> -1

let nth list = function
  | -1 -> Leaf
  | n when List.length list <= n -> Leaf
  | n -> List.nth list n

let rec order_unary list n =
  let rec insert node op = function
    | [] -> []
    | h :: t when h == node -> insert node op t
    | h :: t when h == op -> (p_node node op) :: (insert node op t)
    | h :: t -> h :: (insert node op t)
  and compare node left_op right_op list =
    match (prevalence left_op, prevalence right_op) with
    | (-1, -1) -> list
    | (-1, _) -> insert node right_op list
    | (_, -1) -> insert node left_op list
    | (a, b) when a >= b -> insert node left_op list
    | (a, b) -> insert node right_op list                  
  in if n >= List.length list then list
     else match List.nth list n with
          | (Elem(Unary, l)) ->
             let e = compare (List.nth list n) (nth list (n - 1)) (nth list (n + 1)) list
             in order_unary e (n + 1)
          | _ -> order_unary list (n + 1)


(* le point le plus bas *)
let rec order_op list =
  let rec remove op rep rem = function
    | h ::t when h == rem -> remove op rep rem t
    | h ::t when h == rep -> op :: remove op rep rem t
    | h :: t -> h :: remove op rep rem t
    | _ -> []

  and arrange left node right =
    match (prevalence left, prevalence right) with
    | (a, c) when a >= c ->
       let t = p_node node left
       in remove t left node list

    | (a, c) ->
       let t = l_p_node node right
       in remove t right node list

  and deepest_point v b n = function
    | h :: t when v < prevalence h -> deepest_point (prevalence h) n (n + 1) t
    | h :: t -> deepest_point v b (n + 1) t
    | _ -> b

  in match List.length list with
     | 1 -> list
     | e ->
        let n = deepest_point (-1) 0 0 list in
        let left = nth list (n - 1)
        and node = nth list n
        and right = nth list (n + 1) in
        let l = arrange left node right
        in order_op l

(* BINOP *)
let rec check_expr = function
  | (Elem(Expression, l)) :: t ->
     let list_op = order_unary l 0 in
     let list_child = order_op list_op
     in (Elem(Expression, check_expr list_child )) :: check_expr t
  | (Elem(r, l)) :: t -> (Elem(r, check_expr l )) :: check_expr t
  | _ -> []


(* UNOP *)
let rec check_unop ast =
  let rec get_son = function
    | (Elem(r, l)) :: _ -> l
    | _ -> []
  and get_postfix u = function
    | (Elem(Unop(token), l)) :: t -> get_postfix (u @ [(Elem(Unop(token), []))]) (get_son t)
    | (Elem(Postfix, l)) :: _ -> (Elem(Postfix, check_unop l)) :: u
    | _ -> []
  in match ast with
     | (Elem(Unary, l)) :: t -> (Elem(Unary, get_postfix [] l)) :: check_unop t
     | (Elem(r, l)) :: t -> 
        let i = check_unop l
        in (Elem(r, i)) :: check_unop t
     | _ -> []
