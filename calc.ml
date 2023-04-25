open Ast
open Variable
open Functions

let addition a b = match a, b with
    | VInt(x), VInt(y) -> VInt(x + y)

(* Points to the implementation of the function *)
let fun_of_bop = function
    | BAdd -> addition

(* Evaluate the tree, not everything is already implemented so the pattern matching is not exhaustive*)
let rec eval = function
    | EInt(i) -> VInt(i)
    | EFloat(x) -> VFloat(x)
    | EBop(op, e1, e2) -> (fun_of_bop op) (eval e1) (eval e2);;

let rec vars = function
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBop(op, e1, e2) -> (vars e1) @ (vars e2);;

