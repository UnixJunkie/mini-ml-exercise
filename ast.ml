
type const = True | False | Int of int

type var = Name of string

type bin_op = Plus | Minus | Mult | Div | And | Or

type expr =
  | Const of const
  | Var of var
  | Bin_op of expr * bin_op * expr
  | Apply of expr * expr
  | Fun of (var -> expr)
  | Let of (var * expr * expr) (* bind a variable into an expression *)
  | Let_rec of (string * var * expr * expr)
