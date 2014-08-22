
open Printf

module L = List

(* basic types *)

type const = True | False | Int of int

type var = string

(* variable with its De Bruijn index *)
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
  | DB_var of var * int
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

(* return the DBI of var v *)
let rec find_dbi (v: var) (vars: db_var list) = match vars with
  | [] -> -1 (* a free variable *)
  | (u, i) :: rest ->
    if u = v then i
    else find_dbi v rest

(* add a new variable into the DBI vars list *)
let add_dbi (v: var) (vars: db_var list) =
  (v, 0) ::
  L.map
    (fun (v', i) -> (v', i + 1))
    vars

(* transform an expression into an expression with De Bruijn indexes
   let x = "a" in x -> 0
   let x = "a" in (let y = "b" in x * y) -> 1 * 0
*)
let rec dbi_indexes (vars: db_var list) (e: expr): db_expr = match e with
  | Const c -> DB_const c
  | Var v -> DB_var (v, find_dbi v vars)
  | Bin_op (e1, op, e2) -> DB_bin_op (dbi_indexes vars e1,
                                      op,
                                      dbi_indexes vars e2)
  | Apply (e1, e2) -> DB_apply (dbi_indexes vars e1,
                                dbi_indexes vars e2)
  | Fun (v, e) -> let new_vars = add_dbi v vars in
                  DB_fun ((v, 0),
                          dbi_indexes new_vars e)
  | Let (v, init_expr, in_expr) ->
    let new_vars = add_dbi v vars in
    DB_let ((v, 0),
            dbi_indexes vars init_expr,
            dbi_indexes new_vars in_expr)
