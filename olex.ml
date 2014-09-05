
(* my lexer in pure OCaml, lex is for wimps ;-) *)

type token =
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

let is_digit = function
  | '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9' -> true
  | _ -> false

(* cut the input string when one or several consecutive blanks are
   encountered  *)
let cleanup input_str =
  failwith "not implemented yet"

let token_of_string = function
  | " " | "\t" | "\n" ->
    failwith "token_of_string: blanks must be processed before"
  | "=" -> Equal
  | "+" -> Plus
  | "-" -> Minus
  | "*" -> Mult
  | "/" -> Div
  | "(" -> Lparen
  | ")" -> Rparen
  | "False" -> False
  | "True" -> True
  | "->" -> Arrow
  | "Let" -> Let
  | "Fun" -> Fun
  | "||" -> Or
  | "&&" -> And
  | "in" -> In
  | int_or_var ->
    match String.length int_or_var with
    | 0 -> failwith "token_of_string: empty int_or_var"
    | n ->
      let c = String.get int_or_var 0 in
      if is_digit c then
        (* we assume it's all digits *)
        Int (int_of_string int_or_var)
      else
        Var int_or_var

let rec tokenize (buffer: string list): token list =
  (* FBR: add EOF at the end *)
  failwith "not implemented yet"
