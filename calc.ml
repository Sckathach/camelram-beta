open Ast


let addition x y = x + y;;
let subtraction x y = x - y;;
let multiplication x y = x * y;;

let ex_op = function
    | BopAdd -> addition
    | BopSub -> subtraction
    | BopMul -> multiplication;;

let rec eval = function
    | EInt(i) -> i
    | EBinOp(op, e1, e2) -> (ex_op op) (eval e1) (eval e2)

