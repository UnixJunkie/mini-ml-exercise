open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    while true do
      let result = Parser.main Lexer.token lexbuf in
      Printf.printf "%s\n%!" (string_of_expr result)
    done
  with Lexer.Eof ->
    exit 0
