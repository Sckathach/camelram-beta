open Ast
open Variable
open Functions

type value =
    | VInt of int
    | VFloat of float

(* Points to the implementation of the function *)
let fun_of_bop = function
    | BAdd -> addition
    | BSub -> subtraction
    | BMul -> multiplication
    | BPow -> pow_int;;

(* Evaluate the tree, not everything is already implemented so the pattern matching is not exhaustive*)
let rec eval = function
    | EInt(i) -> VInt(i)
    | EFloat(x) -> VFloat(x)
    | EBop(op, e1, e2) -> (fun_of_bop op) (eval e1) (eval e2);;

let rec vars = function
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBop(op, e1, e2) -> (vars e1) @ (vars e2);;

