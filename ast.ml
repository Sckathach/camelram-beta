open Printf

type bop = BAdd | BSub | BMul | BDiv | BPow
type uop = UMinus

type expr =
    | EInt      of int
    | EFloat    of float
    | EBop      of bop * expr * expr
    | EUop      of uop * expr
    | EIntegral of expr * expr * expr
    | ELet      of string * expr * expr
    | EVar      of string
    | EFun      of func
and func = string * arg list * expr
and arg =
    | AVar      of string

(* Achtung force_value_to_float*)
type value =
    | VInt      of int
    | VFloat    of float

let pprint_value = function
    | VFloat(x) -> Printf.sprintf "%f" x
    | VInt(x) -> Printf.sprintf "%d" x

(** AFFICHAGE

let pprint_bop = function
    | BopAdd -> "+"
    | BopSub -> "-"
    | BopMul -> "*"
    | BopDiv -> "/";;

let pprint_uop = function
    | UnopMinus -> "-";;

let rec pprint_expr = function
    | EInt(i) ->
        sprintf "EInt(%d)" i
    | EBinOp(bop, e1, e2) ->
        "EBinOp(" ^ pprint_bop bop ^ ", " ^ pprint_expr e1 ^ "," ^ pprint_expr e2 ^ ")"
    | EUnOp(uop, e) ->
        "EUnOp(" ^ pprint_uop uop ^ ", " ^ pprint_expr e ^ ")"
    | EIntegral(e1, e2, e3) ->
        "EIntegral(" ^ pprint_expr e1 ^ ", " ^ pprint_expr e2 ^ ", " ^ pprint_expr e3 ^ ")"
    | ELet(x, e1, e2) ->
        "ELet(" ^ x ^ ", " ^ pprint_expr e1 ^ ",\n" ^ pprint_expr e2 ^ ")"
    | EVar(x) ->
        "EVar(" ^ x ^ ")";;

*)