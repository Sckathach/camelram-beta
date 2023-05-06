open Printf

(**
    TYPE bop
    Binary Operator, used to simplify pattern matching
*)
type bop = BAdd | BSub | BMul | BDiv | BPow

(**
    TYPE uop
    Unary Operator, used to simplify pattern matching
*)
type uop =  UMinus | UExp | ULog | UCos | USin | UTan
            | UAcos | UAsin | UAtan | UCosh | USinh | UTanh
            | UCeil | UFloor | URound | UTrunc

(**
    TYPE expr
    Type of the Abstract Syntax Tree
*)
type expr =
    | EInt          of int
    | EFloat        of float
    | EBop          of bop * expr * expr
    | EUop          of uop * expr
    | EIntegral     of expr * expr * expr
    | EIntegralD    of expr * expr * expr * expr
    | EDerivate     of expr * expr
    | ELet          of string * expr * expr
    | EVar          of string
    | EFun          of func
and func = string * arg list * expr
and arg =
    | AVar      of string

(**
    TYPE value
    Used when the AST is evaluated
*)
type value =
    | VInt      of int
    | VFloat    of float

(**
    FUNCTION force_value_to_float
    @type val force_value_to_float : value -> float = <fun>
*)
let force_value_to_float = function
    | VInt(x) -> float_of_int x
    | VFloat(x) -> x

(**
    FUNCTION pprint_value
    @type val pprint_value : value -> string = <fun>
*)
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