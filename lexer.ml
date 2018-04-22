open Str
open Token

exception Unknown_tag of string
exception Error_lexer of string

(* TO DO quotes literals *)

let rec print_str = function
  | h :: t -> print_endline h; print_str t
  | _ -> print_endline "End"

let next = function
  | h :: t -> t
  | _ -> []

let lexer text =

  (* commentary -> start with # and ends with '\n' *)
  let rec uncomment text =
    Str.global_replace (Str.regexp "#.*\n") "\n" (text ^ "\n")
  and tokenize text = function
    | h :: t ->  let del =
                   match h with
                   | "\\^" -> " ^ "
                   | d -> " " ^ d ^ " "
                 and split = ( Str.split ( regexp ( h ) ) ( " " ^ text ^ " " ) )
                 in tokenize ( String.concat del split ) t
    | _ -> (Str.split ( regexp " " ) text)

  and parse text =
    let rec tok token t = token :: ( parse t )

    and check_char = function
      | "" :: t -> check_char t
      | h :: t when String.length h = 1 ->
         let check = function
           | "'" :: n -> Char(int_of_char (String.get h 0)) :: ( parse n )
           | _ -> raise (Error_lexer "Quote")
         in check (if compare (List.hd t) "" = 0 then (next t) else t) 
      | h :: t -> print_endline h; raise (Error_lexer "Quoted")
      | _ -> raise (Error_lexer "Quote")

    and match_re r s = string_match ( regexp r ) s 0
    and regexp_hexa = "^[-+]?0[xX][0-9a-fA-F]+$"
    and regexp_octal = "^[-+]?0o[0-7]+$"
    and regexp_float = "^[-+]?[0-9]+.[0-9]*$"
    and regexp_int = "^[-+]?[0-9]*$"
    and regexp_value = "^[a-zA-Z.]+[a-zA-Z0-9.]*$"
    in match text with
       | "" :: t -> parse t
       | "ç" :: t -> parse t

       | "\n" :: t -> parse t
       (* command *)
       | "def" :: t -> tok Def t
       | "extern" :: t -> tok Extern t

       (* expr *)
       | "while" :: t -> tok While t
       | "do" :: t -> tok Do t
       | "if" :: t -> tok If t
       | "then" :: t -> tok Then t
       | "else" :: t -> tok Else t
       | "for" :: t -> tok For t
       | "in" :: t -> tok In t

       (* extra *)
       | ":" :: t -> tok Assign t
       | "," :: t -> tok Comma t
       | "#" :: t -> tok Wildcard t
       | ";" :: t -> tok End t
       | "(" :: t -> tok Open t
       | ")" :: t -> tok Close t
       | "'" :: t -> check_char t
       | "\"" :: t -> tok Quote t

       (* operators *)
       | "=" :: t -> tok Equal t
       | "+" :: t -> tok Plus t
       | "-" :: t -> tok Minus t
       | "*" :: t -> tok Time t
       | "/" :: t -> tok Div t
       | "%" :: t -> tok Mod t
       | "<" :: t -> tok Less t
       | ">" :: t -> tok More t
       | "^" :: t -> tok Pow t
       | "|" :: t -> tok Pipe t
       | "&" :: t -> tok And t

       (* unary operators *)
       | "!" :: t -> tok Lognot t
       | "~" :: t -> tok Non t

       (* identifiers *)
       | h :: t when match_re regexp_hexa h -> tok ( Integer(int_of_string h ) ) t
       | h :: t when match_re regexp_octal h -> tok ( Integer(int_of_string h ) ) t
       | h :: t when match_re regexp_value h -> tok ( Name(h) ) t
       | h :: t when match_re regexp_int h -> tok ( Integer(int_of_string h) ) t
       | h :: t when match_re regexp_float h -> tok ( Double(float_of_string h) ) t
       | h :: t -> raise (Unknown_tag h)
       | [] -> []

  and separators = ["ç"; "\""; "'"; "|"; "&"; "<"; ">";"("; ")";
                    ":"; ";"; ","; "!"; "~"; "+"; "-"; "*"; "/"; "%";
                    "\\^"; "\n"; "\t"; "\r" ] in
  let t = ( tokenize (uncomment text) separators ) in
  let tokens = ( parse t ) @ [Eof]
  in tokens
