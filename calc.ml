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

let e = 2;;


let rec vars = function
    | EFloat(x) -> []
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBop(op, e1, e2) -> (vars e1) @ (vars e2);;

let rec find_arg x l = match l with
    [] -> None
    | (AVar(y), z) :: q -> if x = y then (Some z) else find_arg x q;;

let rec eval_with_args e args = match e with
    | EInt(x) -> VInt(x)
    | EFloat(x) -> VFloat(x)
    | EVar(x) -> begin
        match find_arg x args with
            | None -> failwith "Non"
            | Some(y) -> y
        end
    | EBop(op, e1, e2) -> (fun_of_bop op) (eval e1) (eval e2);;

(* TESTS *)
let test_e = EBop(BAdd, EVar "x", EVar "y");;
let test_args = [(AVar("x"), VInt(8)); (AVar("y"), VInt(1))]

