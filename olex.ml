
(* my lexer in pure OCaml, that's only OK for a very simple grammar *)

open Printf

module L = List

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

let string_of_token = function
  | Int i  -> sprintf "Int %d" i
  | False  -> "False"
  | True   -> "True"
  | Arrow  -> "Arrow"
  | Let    -> "Let"
  | Fun    -> "Fun"
  | Or     -> "Or"
  | And    -> "And"
  | In     -> "In"
  | Var v  -> v
  | Equal  -> "Equal"
  | Plus   -> "Plus"
  | Minus  -> "Minus"
  | Mult   -> "Mult"
  | Div    -> "Div"
  | Lparen -> "Lparen"
  | Rparen -> "Rparen"
  | Eof    -> "Eof"
                      
(* is_blank : char -> bool *)
let is_blank = function
  | ' ' | '\t' | '\n' -> true
  | _ -> false        
                      
let is_digit = function
  | '0'..'9' -> true
  | _ -> false

let is_lowcase_char = function
  | 'a'..'z' -> true
  | _ -> false

(* prepare input for tokenization *)
let preproc (input_str: string): string list =
  (* surround one char keywords with spaces *)
  let s = Str.global_replace (Str.regexp "[=+-*/()]") " \\0 " input_str in
  (* split into keyword strings *)
  Str.split (Str.regexp "[\n\t ]+") s

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
  | "let" -> Let
  | "fun" -> Fun
  | "||" -> Or
  | "&&" -> And
  | "in" -> In
  | int_or_var ->
    match String.length int_or_var with
    | 0 -> failwith "token_of_string: empty int_or_var"
    | _ ->
      let c = String.get int_or_var 0 in
      if is_digit c then
        (* we assume it's all digits *)
        Int (int_of_string int_or_var)
      else if is_lowcase_char c then
        Var int_or_var
      else
        failwith ("token_of_string: neither int nor var: " ^ int_or_var)

let tokenize (input: string): token list =
  L.map token_of_string (preproc input)
  @
  [Eof]
