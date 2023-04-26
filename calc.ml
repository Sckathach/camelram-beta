open Ast
open Variable
open Functions

(* Points to the implementation of the function *)
let fun_of_bop = function
    | BAdd -> Functions.add
    | BSub -> Functions.sub
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
    | EBop(op, e1, e2) -> (fun_of_bop op) (eval_with_args args e1) (eval_with_args args e2)
    | ELet(x, value, e) -> Variable.add x (eval_with_args args value);
        eval_with_args args e

let eval = eval_with_args []

(* TESTS *)
let test_e = ELet("x", EInt(2), EBop(BPow, EInt(2), EBop(BMul, EVar("x"), EBop(BAdd, EVar "x", EVar "y"))))
let test_args = [(AVar("y"), VInt(1))]

let rec find x = function
    [] -> false
    | y :: q -> if x = y then true else find x q

let seek_mute_var expr =
    let rec aux = function
        | EInt(x) -> [], []
        | EFloat(x) -> [], []
        | EVar(x) ->
            begin
                match (Variable.get x) with
                    | Some(z) -> [], []
                    | None -> [x], []
            end
        | EBop(op, e1, e2) ->
            begin
                match (aux e1), (aux e2) with
                    | (a, b), (c, d) -> a @ c, b @ d
            end
        | ELet(x, value, e) ->
            begin
                match (aux value), (aux e) with
                    | (a, b), (c, d) -> a @ c, x :: b @ d
            end
    in
    let rec aux2 l = function
        | [] -> []
        | x :: q -> if find x l then aux2 q l else x :: (aux2 q l)
    in
        let a, b = aux expr in aux2 b a






