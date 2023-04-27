open Ast
open Variable
open Functions

(* Points to the implementation of the function *)
let fun_of_bop = function
    | BAdd -> Functions.add
    | BSub -> Functions.sub
    | BMul -> Functions.mult
    | BDiv -> Functions.div
    | BPow -> Functions.power

let fun_of_uop = function
    | UMinus -> Functions.minus
    | UExp -> Functions.exp
    | ULog -> Functions.log
    | UCos -> Functions.cos
    | USin -> Functions.sin
    | UTan -> Functions.tan
    | UAcos -> Functions.acos
    | UAsin -> Functions.asin
    | UAtan -> Functions.atan
    | UCosh -> Functions.cosh
    | USinh -> Functions.sinh
    | UTanh -> Functions.tanh
    | UCeil -> Functions.ceil
    | UFloor -> Functions.floor
    | URound -> Functions.round
    | UTrunc -> Functions.trunc

let rec vars = function
    | EFloat(x) -> []
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBop(op, e1, e2) -> (vars e1) @ (vars e2)

let rec find_arg x l = match l with
    [] -> None
    | (AVar(y), z) :: q -> if x = y then (Some z) else find_arg x q

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
        | EUop(_, e) -> aux e
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
                            | None -> failwith "ERREUR : Variable introuvable"
                    end
        end
    | EBop(op, e1, e2) -> (fun_of_bop op) (eval_with_args args e1) (eval_with_args args e2)
    | EUop(op, e) -> (fun_of_uop op) (eval_with_args args e)
    | ELet(x, value, e) -> Variable.add x (eval_with_args args value);
        eval_with_args args e
    | EIntegral(e1, e2, e3) ->
        let dummies = seek_mute_var e3 in
           if List.length dummies != 1 then
                failwith "ERREUR : Il faut une variable muette pour l'intégrale"
           else
                eval_with_args args (EIntegralD(e1, e2, e3, EVar(List.hd dummies)))
    | EIntegralD(e1, e2, e3, EVar(d)) ->
        let inf, sup = (force_value_to_float (eval_with_args args e1)), (force_value_to_float (eval_with_args args e2)) in
        if inf > sup then
            eval_with_args args (EUop(UMinus, EIntegralD(e2, e1, e3, EVar(d))))
        else
        if inf = 0. then
            eval_with_args args (EIntegralD(EFloat(Float.epsilon), e2, e3, EVar(d)))
        else
        if sup = Float.infinity then
           eval_with_args args (EIntegralD(EFloat(inf), EFloat(10000.), e3, EVar(d)))
        else
        if inf = Float.neg_infinity then
            eval_with_args args (EIntegralD(EFloat(-10000.), EFloat(sup), e3, EVar(d)))
        else
            let eps = 0.01 in
            let y = ref inf in
            let f x = eval_with_args ((AVar(d), VFloat(x))::args) e3 in
            let s = ref (f inf) in
            while !y < sup do
                s := Functions.add !s (f !y);
                y := !y +. eps
            done;
            VFloat ((force_value_to_float !s) *. eps)
    | EIntegralD(_, _, _, _) -> failwith "ERREUR : Syntaxe dans l'intégrale"

let eval = eval_with_args []

(* TESTS *)
let test_e = ELet("x", EInt(2), EBop(BPow, EInt(2), EBop(BMul, EVar("x"), EBop(BAdd, EVar "x", EVar "y"))))
let test_args = [(AVar("y"), VInt(1))]









