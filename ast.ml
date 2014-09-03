
module L = List
module P = Printf

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

let const_of_bool = function
  | false -> False
  | true -> True

let string_of_var name =
  name

let string_of_db_var v i =
  P.sprintf "(%s, %d)" v i

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

let rec string_of_db_expr (e: db_expr) = match e with
  | DB_const c -> string_of_const c
  | DB_var (v, i) -> string_of_db_var v i
  | DB_bin_op (e1, op, e2) ->
    (string_of_db_expr e1) ^ " " ^
    (string_of_bin_op op) ^ " " ^
    (string_of_db_expr e2)
  | DB_apply (e1, e2) ->
    (string_of_db_expr e1) ^ " " ^
    (string_of_db_expr e2)
  | DB_fun (v, e) ->
    "fun " ^ (string_of_db_expr (DB_var v)) ^
    " -> " ^ (string_of_db_expr e)
  | DB_let (v, init_expr, in_expr) ->
    "let " ^ (string_of_db_expr (DB_var v)) ^
    " = "  ^ (string_of_db_expr init_expr) ^
    " in " ^ (string_of_db_expr in_expr)

type value =
  | Val_const of const
  | Val_fun of db_var * db_expr

let string_of_value = function
  | Val_const c -> string_of_const c
  | Val_fun (v, e) -> "(" ^ (string_of_db_expr (DB_fun (v, e))) ^ ")"

(* program state *)
type state = value list

(* retrieve variable at index 'i' in 'state' *)
let rec lookup (i: int) (state: state): value =
  if i < 0 then failwith (P.sprintf "lookup %d" i);
  match state with
  | [] -> failwith (P.sprintf "lookup %d []" i)
  | s :: ss ->
    if i = 0 then s
    else lookup (i - 1) ss

(* supported types in constants *)
type const_type =
  | CT_bool of bool
  | CT_int of int

let type_const (c: value): const_type = match c with
  | Val_const True    -> CT_bool true
  | Val_const False   -> CT_bool false
  | Val_const (Int i) -> CT_int i
  | _ -> failwith "type_const tried on Val_fun"

let apply op e1 e2 = match (type_const e1, type_const e2) with
  | (CT_bool b1, CT_bool b2) ->
    (match op with
     | And -> Val_const (const_of_bool (b1 && b2))
     | Or  -> Val_const (const_of_bool (b1 || b2))
     | _ -> failwith (P.sprintf "%s applied on booleans" (string_of_bin_op op))
    )
  | (CT_int   _, CT_bool _) -> failwith "type mismatch: int with bool"
  | (CT_bool  _, CT_int  _) -> failwith "type mismatch: bool with int"
  | (CT_int  i1, CT_int i2) ->
    (match op with
     | Plus -> Val_const (Int (i1 + i2))
     | Minus -> Val_const (Int (i1 - i2))
     | Mult -> Val_const (Int (i1 * i2))
     | Div -> Val_const (Int (i1 / i2))
     | _ -> failwith (P.sprintf "%s applied on integers" (string_of_bin_op op))
    )

let interpret (ex: db_expr): value =
  let rec loop (s: state) (e: db_expr): value = match e with
    | DB_const c -> Val_const c
    | DB_fun (v, e) -> Val_fun (v, e)
    | DB_var (_v, i) -> lookup i s
    | DB_let (v, init_expr, in_expr) ->
      let new_v = loop s init_expr in
      loop (new_v :: s) in_expr
    | DB_apply (e1, e2) ->
      (match e1 with
       | DB_fun (v, e') ->
         let param = loop s e2 in
         loop (param :: s) e'
       | _ -> failwith "only a function can be applied"
      )
    | DB_bin_op (e1, op, e2) ->
      let v1 = loop s e1 in
      let v2 = loop s e2 in
      apply op v1 v2
  in
  loop [] ex

type instruction =
  | Access of int      (* acces a var in the env *)
  | Apply              (* apply a function *)
  | Cur of instruction list (* body of a function *)
  | Return             (* return from a function *)
  | Let                (* add var to env *)
  | Branchneg of int   (* conditional branch *)
  | Branch of int      (* unconditional branch *)
  | Op of bin_op       (* binary operation *)
  | Push of const_type (* push val on the stack *)

type closure = instruction list * val_or_closure list
and val_or_closure =
  | Val of const
  | Clo of closure

type vm_state =
  instruction list * (* code *)
  val_or_closure list * (* env *)
  value list * (* exec stack *)
  instruction list list (* call stack *)
