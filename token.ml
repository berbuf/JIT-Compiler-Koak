type token =
  (* command *)
  | Def
  | Extern

  (* operator *)
  | Equal
  | Plus 
  | Minus
  | Time
  | Div
  | Mod
  | Less
  | More
  | Pow
  | Pipe
  | And

  (* unary operator *)
  | Lognot
  | Non

  (* expr *)
  | While
  | Do
  | If
  | Then
  | Else
  | For
  | In

  (* identifiers *)
  | Name of string
  | Double of float
  | Integer of int
  | Char of int

  (* extra *)
  | Eof
  | Comma
  | Assign 
  | End
  | Open
  | Close
  | Wildcard
  | Quote
  | Doublequote
