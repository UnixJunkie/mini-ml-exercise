open Ast

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.main Lexer.token lexbuf in
    Printf.printf "input: %s\n" (string_of_expr e);
    let db_e = dbi_indexes [] e in
    Printf.printf "output: %s\n" (string_of_db_expr db_e)
  with Lexer.Eof ->
    exit 0
