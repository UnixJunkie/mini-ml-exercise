
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
  sprintf "(%s, %d)" v i

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
(* FBR: simplifier *)
let rec find_dbi (v: var) (vars: db_var list) = match vars with
  | [] -> failwith ("find_dbi: cannot find: " ^ (string_of_var v))
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
   let x = "a" in (let y = "b" in x * y) -> 1 * 0 *)
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

let const_of_value = function
  | Val_const c -> c
  | Val_fun _ -> failwith "const_of_value: cannot work on a Val_fun"

let string_of_value = function
  | Val_const c -> string_of_const c
  | Val_fun (v, e) -> "(" ^ (string_of_db_expr (DB_fun (v, e))) ^ ")"

(* program state *)
type state = value list

(* retrieve variable at index 'i' in 'state' *)
let rec lookup (i: int) (state: state): value =
  if i < 0 then failwith (sprintf "lookup %d" i);
  match state with
  | [] -> failwith (sprintf "lookup %d []" i)
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
     | _ -> failwith (sprintf "%s applied on booleans" (string_of_bin_op op))
    )
  | (CT_int   _, CT_bool _) -> failwith "type mismatch: int with bool"
  | (CT_bool  _, CT_int  _) -> failwith "type mismatch: bool with int"
  | (CT_int  i1, CT_int i2) ->
    (match op with
     | Plus -> Val_const (Int (i1 + i2))
     | Minus -> Val_const (Int (i1 - i2))
     | Mult -> Val_const (Int (i1 * i2))
     | Div -> Val_const (Int (i1 / i2))
     | _ -> failwith (sprintf "%s applied on integers" (string_of_bin_op op))
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
         (* FBR: this will have to be fixed so that we can do partial
                 application of functions
            e.g.  echo "let f = fun x -> (fun y -> x + y) in (f 3) 4" | ./test
         *)
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
  | Push of const      (* push val on the stack *)

let string_of_list to_string sep l =
  "[" ^ String.concat sep (L.map to_string l) ^ "]"

let rec string_of_instruction = function
  | Access i -> sprintf "Access %d" i
  | Apply -> "Apply"
  | Cur instrs -> "Cur " ^ string_of_instructions instrs
  | Return -> "Return"
  | Let -> "Let"
  | Branchneg i -> sprintf "Branchneg %d" i
  | Branch i -> sprintf "Branch %d" i
  | Op op -> sprintf "Op %s" (string_of_bin_op op)
  | Push c -> sprintf "Push %s" (string_of_const c)
and string_of_instructions instructions =
  string_of_list string_of_instruction " " instructions

type closure = instruction list * val_or_closure list
and val_or_closure =
  | Val of const
  | Clo of closure

let rec string_of_closure ((code, env): closure): string =
  "(" ^ (string_of_instructions code)
  ^ ", "
  ^ (string_of_val_or_closures env)
  ^ ")"
and string_of_val_or_closure = function
  | Val v -> string_of_const v
  | Clo c -> string_of_closure c
and string_of_val_or_closures vocs =
  string_of_list string_of_val_or_closure " " vocs
and string_of_closures cs =
  string_of_list string_of_closure " " cs

type vm_state =
  instruction list *    (* code *)
  val_or_closure list * (* env *)
  val_or_closure list * (* exec stack *)
  closure list          (* call stack *)

let string_of_vm_state ((c, e, s, r): vm_state): string =
  "code: "       ^ string_of_instructions c    ^ "\n" ^
  "env: "        ^ string_of_val_or_closures e ^ "\n" ^
  "exec_stack: " ^ string_of_val_or_closures s ^ "\n" ^
  "call_stack: " ^ string_of_closures r        ^ "\n"

(* skip 'n' elements in 'l' *)
let skip n l =
  if n < 0 then failwith (sprintf "skip: n: %d" n);
  let rec loop i lst =
    if i = 0 then lst
    else
      match lst with
      | x :: xs -> loop (i - 1) xs
      | _ -> failwith (sprintf "skip: too much: i: %d" i)
  in
  loop n l

let rec execute (cesr: vm_state) = match cesr with
  | ([], e, s, r) -> ([], e, s, r) (* FBR: not sure what to do here *)
  | (Access n :: c, e, s, r) ->
    execute (c, e, [L.nth e n], r)
  | (Apply :: c, e, Clo (c0, e0) :: Val v :: s, r) ->
    execute (c0, Val v :: e0, s, (c, e) :: r)
  | (Apply :: c, e, _, r) ->
    failwith "execute: cannot apply"
  | (Cur c' :: c, e, s, r) ->
    execute (c, e, [Clo (c', e)], r)
  | (Return :: c, e, s, (c0, e0) :: r) ->
    execute (c0, e0, s, r)
  | (Return :: c, e, s, _) ->
    failwith "execute: cannot Return"
  | (Let :: c, e, v :: s, r) ->
    execute (c, v :: e, s, r)
  | (Let :: c, e, [], r) ->
    failwith "execute: cannot Let"
  | (Branch n :: c, e, s, r) ->
    execute (skip n c, e, s, r)
  | (Branchneg n :: c, e, Val True :: s, r) ->
    execute (c, e, s, r) (* exec next instr *)
  | (Branchneg n :: c, e, Val False :: s, r) ->
    execute (skip n c, e, s, r) (* skip next n instr *)
  | (Branchneg n :: c, e, _, r) -> failwith "execute: cannot branch"
  | (Op op :: c, e, Val v :: Val w :: s, r) ->
    (* FBR: maybe :: s is missing in there and in the spec. *)
    execute (c, e,
             [Val (const_of_value (apply op (Val_const v) (Val_const w)))],
             r)
  | (Op op :: c, e, _, r) ->
    failwith "execute: cannot op"
  | (Push v :: c, e, s, r) ->
    execute (c, e, Val v :: s, r)

(* the compiler *)
let rec compile (program: db_expr) (acc: instruction list): instruction list =
  match program with (* the interpreter code is in comments *)
  | DB_const c -> (* Val_const c *) (Push c) :: acc (* terminal case *)
    (* FBR: il va bien falloir renverser l'accu a un moment *)
  | DB_fun (_v, e) -> (* Val_fun (v, e) *) (Cur (compile e [])) :: acc
  | DB_var (_v, i) -> (* lookup i s *) (Access i) :: acc
  | DB_let (v, init_expr, in_expr) ->
    (* let new_v = loop s init_expr in loop (new_v :: s) in_expr *)
    compile in_expr (compile init_expr acc)
  | DB_apply (e1, e2) ->
    (* (match e1 with *)
    (*  | DB_fun (v, e') -> let param = loop s e2 in loop (param :: s) e' *)
    (*  | _ -> failwith "only a function can be applied" *)
    (* ) *)
    Apply :: compile e1 (compile e2 acc)
  | DB_bin_op (e1, op, e2) ->
    (* let v1, v2 = loop s e1, loop s e2 in apply op v1 v2 *)
    Op op :: compile e1 (compile e2 acc)
