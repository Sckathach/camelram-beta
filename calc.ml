open Ast
open Variable
open Functions

(* Points to the implementation of the function *)
let fun_of_bop = function
    | BAdd -> Functions.add
    | BMul -> Functions.mult
    | BPow -> Functions.power

let rec vars = function
    | EFloat(x) -> []
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBop(op, e1, e2) -> (vars e1) @ (vars e2)

let rec find_arg x l = match l with
    [] -> None
    | (AVar(y), z) :: q -> if x = y then (Some z) else find_arg x q

let rec eval_with_args args = function
    | EInt(x) -> VInt(x)
    | EFloat(x) -> VFloat(x)
    | EVar(x) ->
        begin
            match (find_arg x args) with
                | Some(y) -> y
                | None ->
                    begin
                        match (Variable.get x) with
                            | Some(z) -> z
                            | None -> failwith "Non"
                    end
        end
    | EBop(op, e1, e2) -> (fun_of_bop op) (eval_with_args e1 args) (eval_with_args e2 args)

let eval = eval_with_args []

(* TESTS *)
let test_e = EBop(BPow, EInt(2), EBop(BMul, EVar("x"), EBop(BAdd, EVar "x", EVar "y")))
let test_args = [(AVar("y"), VInt(1))]
