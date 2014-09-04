open Ast
open Printf

let _ =
  try
    let lexbuf = Lexing.from_channel stdin in
    let e = Parser.main Lexer.token lexbuf in
    printf "input: %s\n" (string_of_expr e);
    let db_e = dbi_indexes [] e in
    printf "output: %s\n" (string_of_db_expr db_e);
    (try
       let ires = interpret db_e in
       printf "res: %s\n" (string_of_value ires);
     with Error msg ->
       printf "interpretation failure: %s\n" msg
    );
    let compiled = compile db_e in
    let cres = run compiled in
    printf "compiled: %s\n" (string_of_vm_state cres)
  with Lexer.Eof ->
    exit 0
