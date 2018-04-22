open Lexer
open Grammar
open Operator
open Function_type
open Misc_type
open Check_type
open Ast
open Asm

let read_file filename = 
  let lines = ref []
  and fd = open_in filename
  in try while true; do
           lines := input_line fd :: !lines
         done; ""
     with End_of_file -> close_in fd; (String.concat "\n" (List.rev !lines)) ;;
;;

let compile text filename =
  let tokens = lexer text in
  let root = [ast tokens] in
  let op_root = check_expr root in
  let unop_root = check_unop op_root in
  let type_root = check_type unop_root
  in build_asm type_root filename
;;

let rec prompt line =
  compile line "";
  print_string "> "; 
  prompt (read_line ())
;;

let rec main () =
  begin
    try 
      if Array.length Sys.argv > 1 then
        let text = read_file (Sys.argv.(1))
        in print_endline text;
           compile text Sys.argv.(1);
           exit 0
      else
        let _ = print_string "> " 
        in prompt (read_line ())
    with
    | Error_lexer(e) -> print_endline ("Error lexer " ^ e)
    | Unknown_tag(e) -> print_endline ("Unknown tag " ^ e)
    | Unknown_type(e) -> print_endline ("Unknown type " ^ e)
    | Type_error(e) -> print_endline ("Type error " ^ e)
    | Undefined_function(e) -> print_endline ("Undefined function " ^ e)
    | Already_defined(e) -> print_endline ("Already defined function " ^ e)
    | Undefined_variable(e) -> print_endline ("Undefined variable " ^ e)
    | Incoherent_signature(e) -> print_endline ("Incoherent types " ^ e)
    | Syntax_error(e) -> print_endline ("Syntax error " ^ e)
    | Sys_error(e) -> print_endline e
    | End_of_file -> exit 0
  end;
  if Array.length Sys.argv > 1 then exit 0
  else main ()
;;

let () = main () ;;
