
open Printf

module L = List

(* basic types *)

type const = True | False | Int of int

type var = string

(* variable with its De Bruijn index *)
type db_var = int

type bin_op = Plus | Minus | Mult | Div | And | Or

exception Error of string

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

let string_of_db_var =
  string_of_int

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
let find_dbi (v: var) (vars: var list): int =
  let rec loop i vs = match vs with
    | [] -> raise @@ Error ("find_dbi: cannot find: " ^ (string_of_var v))
    | u :: us -> if u = v then i else loop (i + 1) us
  in
  loop 0 vars

(* transform an expression into an expression with De Bruijn indexes
   let x = a in x -> 0
   let x = a in (let y = b in x * y) -> 1 * 0 *)
let rec dbi_indexes (vars: var list) (e: expr): db_expr = match e with
  | Const c -> DB_const c
  | Var v -> DB_var (find_dbi v vars)
  | Bin_op (e1, op, e2) -> DB_bin_op (dbi_indexes vars e1,
                                      op,
                                      dbi_indexes vars e2)
  | Apply (e1, e2) -> DB_apply (dbi_indexes vars e1,
                                dbi_indexes vars e2)
  | Fun (v, e) -> DB_fun (0, dbi_indexes (v :: vars) e)
  | Let (v, init_expr, in_expr) ->
    DB_let (0,
            dbi_indexes vars init_expr,
            dbi_indexes (v :: vars) in_expr)

let rec string_of_db_expr (e: db_expr) = match e with
  | DB_const c -> string_of_const c
  | DB_var i -> string_of_db_var i
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
    | DB_const c                     -> Val_const c
    | DB_fun (v, e)                  -> Val_fun (v, e)
    | DB_var i                       -> lookup i s
    | DB_let (v, init_expr, in_expr) ->
      let new_v = loop s init_expr in
      loop (new_v :: s) in_expr
    | DB_apply (e1, e2)              ->
      (let param = loop s e2 in
       match e1 with
       | DB_fun (_v, e') -> loop (param :: s) e'
       (* FBR: really no idea of what to do here *)
       | _ -> failwith "interpret: can only apply a function"
      );
    | DB_bin_op (e1, op, e2)         -> apply op (loop s e1) (loop s e2)
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
  string_of_list string_of_instruction "; " instructions

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
  string_of_list string_of_val_or_closure "; " vocs
and string_of_closures cs =
  string_of_list string_of_closure "; " cs

type vm_state =
  instruction list *    (* code *)
  val_or_closure list * (* env *)
  val_or_closure list * (* exec stack *)
  closure list          (* call stack *)

let string_of_vm_state ((c, e, s, r): vm_state): string =
  "---\n" ^
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

let rec access i l =
  if i < 0 then failwith (sprintf "access: negative index: %d" i);
  match l with
  | x :: xs -> if i = 0 then x else access (i - 1) xs
  | [] -> failwith "access: empty list"

let rec execute (cesr: vm_state) =
  (* printf "%s\n" (string_of_vm_state cesr); (\* debug trace *\) *)
  match cesr with
  | ([], e, s, []) ->
    ([], e, s, [])
  | ([], e, s, (c0, e0) :: rs) ->
      execute (c0, e0, s, rs)
  | (Access n :: c, e, s, r) ->
      execute (c, e, (access n e) :: s, r)
  | (Apply :: c, e, Clo (c0, e0) :: Val v :: s, r) ->
      execute (c0, Val v :: e0, s, (c, e) :: r)
  | (Apply :: c, e, Clo (c0, e0) :: s, r) -> (* partial application *)
      execute (c0, e0, s, (c, e) :: r)
  | (Apply :: c, e, _, r) -> failwith "execute: cannot apply"
  | (Cur c' :: c, e, s, r) ->
      execute (c, e, Clo (c', e) :: s, r)
  | (Return :: c, e, s, (c0, e0) :: r) ->
      execute (c0, e0, s, r)
  | (Return :: c, e, s, _) -> failwith "execute: cannot Return"
  | (Let :: c, e, v :: s, r) ->
      execute (c, v :: e, s, r)
  | (Let :: c, e, [], r) -> failwith "execute: cannot Let"
  | (Branch n :: c, e, s, r) ->
      execute (skip n c, e, s, r)
  | (Branchneg n :: c, e, Val True :: s, r) ->
      execute (c, e, s, r) (* exec next instr *)
  | (Branchneg n :: c, e, Val False :: s, r) ->
      execute (skip n c, e, s, r) (* skip next n instr *)
  | (Branchneg n :: c, e, _, r) -> failwith "execute: cannot branch"
  | (Op op :: c, e, Val v :: Val w :: s, r) ->
      execute (c, e,
               Val (const_of_value (apply op (Val_const v) (Val_const w))) :: s,
               r)
  | (Op op :: c, e, _, r) -> failwith "execute: cannot op"
  | (Push v :: c, e, s, r) ->
      execute (c, e, Val v :: s, r)

(* the compiler *)
let rec compile (program: db_expr): instruction list =
  let rec loop (prog: db_expr) (acc: instruction list): instruction list =
    match prog with
    | DB_const c             -> (Push c) :: acc
    | DB_fun (_v, e)         -> (Cur (compile e)) :: acc
    | DB_var i               -> (Access i) :: acc
    | DB_let (_v,
              init_expr,
              in_expr)       -> loop in_expr (Let :: (loop init_expr acc))
    | DB_apply (e1, e2)      -> Apply :: loop e1 (loop e2 acc)
    | DB_bin_op (e1, op, e2) -> Op op :: loop e1 (loop e2 acc)
  in
  (* return instructions in the correct order *)
  L.rev @@ loop program []

let run (instrs: instruction list): vm_state =
  execute (instrs, [], [], [])
