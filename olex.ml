
(* my lexer in pure OCaml, lex is for wimps ;-) *)

type token =
  | Blank (* ' '|'\t'|'\n' *)
  | Int of int
  | False
  | True
  | Arrow
  | Let
  | Fun
  | Or
  | And
  | In
  | Var of string
  | Equal
  | Plus
  | Minus
  | Mult
  | Div
  | Lparen
  | Rparen
  | Eof

(* is_blank : char -> bool *)
let is_blank = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false

(* cut the input string when one or several consecutive blancks are
   encountered  *)
let cleanup input_str =
  failwith "not implemented yet"

let rec tokenize (buffer: string list): token list =
  (* FBR: add EOF at the end *)
  failwith "not implemented yet"
