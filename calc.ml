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

let force_value_to_float = function
    | VInt(x) -> float_of_int x
    | VFloat(x) -> x

let rec find x = function
    [] -> false
    | y :: q -> if x = y then true else find x q

let seek_mute_var expr =
    let rec aux = function
        | EVar(x) ->
            begin
                match (Variable.get x) with
                    | Some(_) -> [], []
                    | None -> [x], []
            end
        | EBop(_, e1, e2) ->
            begin
                match (aux e1), (aux e2) with
                    | (a, b), (c, d) -> a @ c, b @ d
            end
        | ELet(x, e1, e2) ->
            begin
                match (aux e1), (aux e2) with
                    | (a, b), (c, d) -> a @ c, x :: b @ d
            end
        | _ -> [], []
    in
    let rec aux2 acc l = function
        | [] -> acc
        | x :: q -> if find x l || find x acc then aux2 acc l q else aux2 (x :: acc) l q
    in
        let a, b = aux expr in (aux2 [] b a)

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
    | EIntegral(e1, e2, e3) ->
        let inf, sup = (force_value_to_float (eval_with_args args e1)), (force_value_to_float (eval_with_args args e2)) in
        if inf > sup then
            eval_with_args args (EIntegral(e2, e1, e3))
        else
            let dummies = seek_mute_var e3 in
            if List.length dummies != 1 then
                failwith "Il faut une variable muette pour l'intégrale"
            else
                let arg = List.hd dummies in
                let eps = 0.01 in
                let y = ref inf in
                let f x = eval_with_args ((AVar(arg), VFloat(x))::args) e3 in
                let s = ref (f inf) in
                while !y < sup do
                    s := Functions.add !s (f !y);
                    y := !y +. eps
                done;
                VFloat ((force_value_to_float !s) *. eps)

let eval = eval_with_args []

(* TESTS *)
let test_e = ELet("x", EInt(2), EBop(BPow, EInt(2), EBop(BMul, EVar("x"), EBop(BAdd, EVar "x", EVar "y"))))
let test_args = [(AVar("y"), VInt(1))]









