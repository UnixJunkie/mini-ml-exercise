
open Printf

(* basic types *)

type const = True | False | Int of int

type var = (*Name of*) string

type bin_op = Plus | Minus | Mult | Div | And | Or

(* expressions *)

type expr =
  | Const of const
  | Var of var
  | Bin_op of expr * bin_op * expr
  | Apply of expr * expr
  | Fun of (var * expr) (* fonction definition *)
  | Let of (var * expr * expr) (* bind a variable into an expression *)
(*
  | Let_rec of (string * var * expr * expr)
*)

(* printers *)

let string_of_const = function
  | True  -> "True"
  | False -> "False"
  | Int i -> string_of_int i

let string_of_var = function
  | name -> name

let string_of_bin_op = function
  | Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | And   -> "&&"
  | Or    -> "||"

let rec string_of_expr = function
  | Const c -> string_of_const c
  | Var v -> string_of_var v
  | Bin_op (e1, op, e2) ->
    (string_of_expr e1)   ^ " " ^
    (string_of_bin_op op) ^ " " ^
    (string_of_expr e2)
  | Apply (e1, e2) -> (string_of_expr e1) ^ " " ^ (string_of_expr e2)
  | Fun (v, e) -> "fun " ^ (string_of_var v) ^ " -> " ^ (string_of_expr e)
  | Let (v, init, in_expr) ->
    "let " ^ (string_of_var v) ^ " = " ^ (string_of_expr init) ^ " in " ^
    (string_of_expr in_expr)
(*
  | Let_rec (fun_name, var, init, in_expr) ->
    "let rec " ^ fun_name ^ " " ^ (string_of_var var) ^ " = " ^
    (string_of_expr init) ^ " in " ^ (string_of_expr in_expr)
*)
