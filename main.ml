open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let result = Parser.main Lexer.token lexbuf in
    Printf.printf "%s\n%!" (string_of_expr result)
  with Lexer.Eof ->
    exit 0
