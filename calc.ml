open Ast

let addition x y = x + y;;
let subtraction x y = x - y;;
let multiplication x y = x * y;;

(* Points to the implementation of the function *)
let ex_op = function
    | BopAdd -> addition
    | BopSub -> subtraction
    | BopMul -> multiplication;;

(* Evaluate the tree, not everything is already implemented so the pattern matching is not exhaustive*)
let rec eval = function
    | EInt(i) -> i
    | EBinOp(op, e1, e2) -> (ex_op op) (eval e1) (eval e2);;

let rec vars = function
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBinOp(op, e1, e2) -> (vars e1) @ (vars e2);;

