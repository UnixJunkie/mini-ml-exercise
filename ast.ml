
open Printf

(* basic types *)

type const = True | False | Int of int

type var = string

(* var with its De Bruijn index *)
type db_var = string * int

type bin_op = Plus | Minus | Mult | Div | And | Or

(* expressions *)

type expr =
  | Const of const
  | Var of var
  | Bin_op of expr * bin_op * expr
  | Apply of expr * expr
  | Fun of (var * expr) (* fonction definition *)
  | Let of (var * expr * expr) (* bind a variable into an expression *)

(* expr with db_var instead of var *)
type db_expr =
  | Const of const
  | Var of db_var
  | Bin_op of expr * bin_op * expr
  | Apply of expr * expr
  | Fun of (db_var * expr)
  | Let of (db_var * expr * expr)

(* printers *)

let string_of_const = function
  | True  -> "True"
  | False -> "False"
  | Int i -> string_of_int i

(* let string_of_var (name, index) = *)
(*   Printf.sprintf "%s_%d" name index *)
let string_of_var name =
  name

let string_of_bin_op = function
  | Plus  -> "+"
  | Minus -> "-"
  | Mult  -> "*"
  | Div   -> "/"
  | And   -> "&&"
  | Or    -> "||"

let rec string_of_expr (e: expr) = match e with
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

let rec compute_db_indexes (e: expr): db_expr = match e with
  | Const cst -> failwith "not implemented yet"
  | Var v -> failwith "not implemented yet"
  | Bin_op (e1, op, e2) -> failwith "not implemented yet"
  | Apply (e1, e2) -> failwith "not implemented yet"
  | Fun (v, e) -> failwith "not implemented yet"
  | Let (v, e1, e2) -> failwith "not implemented yet"
