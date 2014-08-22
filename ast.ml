
open Printf

module L = List

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
  | DB_const of const
  | DB_var of db_var
  | DB_bin_op of db_expr * bin_op * db_expr
  | DB_apply of db_expr * db_expr
  | DB_fun of (db_var * db_expr)
  | DB_let of (db_var * db_expr * db_expr)

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

type dbi_var = var * int

let add_var (v: var) (l: dbi_var list) =
  (v, 0) ::
  (L.map
     (fun (v', i) -> (v', i + 1))
     l)

let dbi_indexes e =
  let loop (seen: dbi_var list) (e: expr): (dbi_var list) =
    failwith "not implemented yet"
  in
  loop [] e
