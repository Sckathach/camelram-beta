open Ast
open Variable
open General
open Function

(**
    FUNCTION fun_of_bop
    @type val fun_of_bop : bop -> value -> value -> value = <fun>
    @returns The function (val fun : value -> value -> value) found in general.ml
*)
let fun_of_bop = function
    | BAdd -> General.add
    | BSub -> General.sub
    | BMul -> General.mult
    | BDiv -> General.div
    | BPow -> General.power

(**
    FUNCTION fun_of_uop
    @type val fun_of_uop : uop -> value -> value = <fun>
    @returns The function (val fun : value -> value) found in general.ml
*)
let fun_of_uop = function
    | UMinus -> General.minus
    | UExp -> General.exp
    | ULog -> General.log
    | UCos -> General.cos
    | USin -> General.sin
    | UTan -> General.tan
    | UAcos -> General.acos
    | UAsin -> General.asin
    | UAtan -> General.atan
    | UCosh -> General.cosh
    | USinh -> General.sinh
    | UTanh -> General.tanh
    | UCeil -> General.ceil
    | UFloor -> General.floor
    | URound -> General.round

(**
    FUNCTION vars
    @type vars : expr -> string list = <fun>
    @returns A list of the variables in the expression
*)
let rec vars = function
    | EFloat(x) -> []
    | EInt(i) -> []
    | EVar(x) -> [x]
    | EBop(op, e1, e2) -> (vars e1) @ (vars e2)

(**
    FUNCTION find_args
    @type val find_args : string -> (arg * 'a) list -> 'a option = <fun>
    @returns The value of the variable x if x is in the list l. l is a (arg * 'a) list, it is used to store dummy
        variables
*)
let rec find_arg x l = match l with
    [] -> None
    | (AVar(y), z) :: q -> if x = y then (Some z) else find_arg x q

(**
    FUNCTION find
    @requires Two arguments, x and a list
    @type val find : 'a -> 'a list -> bool = <fun>
    @returns True if x is in the list, false if not
*)
let rec find x = function
    [] -> false
    | y :: q -> if x = y then true else find x q

(**
    FUNCTION seek_mute_var
    @type val seek_mute_var : expr -> string list = <fun>
    @returns A list of the name of the variables that aren't in the dictionary (variables initialised with let)
*)
let seek_mute_var expr =
    (* aux seeks all the variables, variables alone in a list and let variables in another list (let variables are not
        yet in the dictionary but they aren't dummy variables) *)
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
    (* aux2 removes the elements that are in both lists *)
    let rec aux2 acc l = function
        | [] -> acc
        | x :: q -> if find x l || find x acc then aux2 acc l q else aux2 (x :: acc) l q
    in
        let a, b = aux expr in (aux2 [] b a)

(** FUNCTION eval_with_args
    @type val eval_with_args : (arg * value) list -> expr -> value = <fun>
    @requires All the variables need to be either stored globally with the syntax let x in or locally in the (arg *
    value) list
    @raises Not_Found if a variable is unknown
    @returns The numerical evaluation of the expression if everything is known
    @example An expr tree : ELet("x", EInt(2), EBop(BPow, EInt(2), EBop(BMul, EVar("x"), EBop(BAdd, EVar "x",
     EVar "y")))), a (arg * value) list : [(AVar("y"), VInt(1))], here "x" is stored globally
*)
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
                s := General.add !s (f !y);
                y := !y +. eps
            done;
            VFloat ((force_value_to_float !s) *. eps)
    | EIntegralD(_, _, _, _) -> failwith "ERREUR : Syntaxe dans l'intégrale"

(**
    FUNCTION eval
    @type val eval : expr -> value
    @raises failwith if an argument is needed
*)
let eval = eval_with_args []

(**
    FUNCTION eval_function
    @type val eval_function : expr -> value list -> value = <fun>
    @raises Invalid_argument if the given arg list doesn't have the required length
    @example Eval f(2, 1) -> eval_function f [VInt 2; VInt 1];;
*)
let eval_function f args = match f with
    EFun(_, vars, e) -> eval_with_args (List.combine vars args) e

(**
    FUNCTION eval_function_by_name
    @type val eval_function_by_name : string -> value list -> value = <fun>
    @raises Failure if the function doesn't exist
    @example Eval f(2, 1) -> eval_function_by_name "f" [VInt 2; VInt 1];;
*)
let eval_function_by_name f args = match Function.get f with
    | Some(x) -> eval_function x args
    | None -> failwith "ERREUR : La fonction n'existe pas"

(* TESTS *)
let test_e = ELet("x", EInt(2), EBop(BPow, EInt(2), EBop(BMul, EVar("x"), EBop(BAdd, EVar "x", EVar "y"))))
let test_args = [(AVar("y"), VInt(1))]
let test_f = EFun("f", [AVar "x"; AVar "y"], EBop(BAdd, EVar "x", EVar "y"))

(**
    FUNCTION replace_var
    @type val replace_var : string -> expr -> expr = <fun>
    @raises Failure if the replacement expression provided has zero or more than one dummy variable
    @return The expression where the variable given as a string argument is replaced by the expression containing the
        new variable
*)
let replace_var var rep expr =
    let rec aux a b c = function
        | EVar(a) -> c
        | EInt(x) -> EInt(x)
        | EFloat(x) -> EFloat(x)
        | EUop(x, e) -> EUop(x, aux a b c e)
        | EBop(x, e1, e2) -> EBop(x, aux a b c e1, aux a b c e2)
        | ELet(x, e1, e2) -> if x = a then ELet(b, aux a b c e1, aux a b c e2)
            else ELet(x, aux a b c e1, aux a b c e2)
        | EIntegral(e1, e2, e3) -> EIntegral(aux a b c e1, aux a b c e2, aux a b c e3)
        | EIntegralD(e1, e2, e3, e4) -> EIntegralD(aux a b c e1, aux a b c e2, aux a b c e3, aux a b c e4)
    in
        let l = (seek_mute_var rep) in
            if List.length l > 1 || List.length l = 0 then
                failwith "ERREUR : mauvais argument dans replace_var"
            else
                aux var (List.hd l) rep expr

(**
    FUNCTION list_of_string
    @type val list_of_string : string -> string list = <fun>
*)
let list_of_string s =
    let rec aux i l =
        if i < 0 then l else aux (i - 1) ((String.make 1 s.[i]) :: l) in
    aux (String.length s - 1) []

(**
    FUNCTION split_var
    @type val split_var : expr -> expr = <fun>
    @returns Replaces EVar "ab" with EBop(EMul, EVar "a", EVar "b") if the variable "ab" doesn't exist in the dictionary
        (for example EVar "pi" is replaced by EVar "pi")
*)
let rec split_var = function
    | EVar(a) ->
        begin
            match Variable.get a with
                | None ->
                    begin
                        let rec aux = function
                            | [x] -> EVar x
                            | x :: q -> EBop(BMul, EVar x, aux q)
                        in aux (list_of_string a)
                    end
                | Some(x) -> EVar(a)
        end
    | EInt(x) -> EInt(x)
    | EFloat(x) -> EFloat(x)
    | EUop(x, e) -> EUop(x, split_var e)
    | EBop(x, e1, e2) -> EBop(x, split_var e1, split_var e2)
    | ELet(x, e1, e2) -> ELet(x, split_var e1, split_var e2)
    | EIntegral(e1, e2, e3) -> EIntegral(split_var e1, split_var e2, split_var e3)
    | EIntegralD(e1, e2, e3, e4) -> EIntegralD(split_var e1, split_var e2, split_var e3, e4)


let rec differentiate expr x=
    match expr with
        | EDifferentiate(e1, y) -> EDifferentiate((differentiate e1 x), y)
        (*| EIntegralD(e, _, _, EVar(d)) when d=x -> e
        | EIntegralD(e, bb, bh, d) -> EIntegralD((differentiate e x), (differentiate bb x), (differentiate bh x), d)
        | EIntegral(e, bb ,bh) ->
            begin
                let dummies = seek_mute_var e3 in
                    if List.length dummies != 1 then
                        failwith "ERREUR : Il faut une variable muette pour l'intégrale"
                    else
                        if (List.hd dummies) = x then
                            e
                        else
                            
            end*)
        | EBop(op, e1, e2) -> 
            begin
                match op with
                    | BAdd -> EBop(BAdd, (differentiate e1 x), (differentiate e2 x))
                    | BSub -> EBop(BSub, (differentiate e1 x), (differentiate e2 x))
                    | BMul -> EBop(BAdd, EBop(BMul, (differentiate e1 x), e2), EBop(BMul, e1, (differentiate e2 x)))
                    | BDiv -> EBop(BDiv, EBop(BSub, EBop(BMul, (differentiate e1 x), e2), EBop(BMul, e1, (differentiate e2 x))), EBop(BMul, e2, e2))
                    | BPow -> EBop(BAdd, EBop(BMul, EBop(BMul, EUop(ULog, e1), EBop(BPow, e1, e2)), (differentiate e2 x)), EBop(BMul, EBop(BMul, e2, EBop(BPow, e1, EBop(BSub, e2, EInt(1)))), (differentiate e1 x)))
            end
        | EUop(op, e1) ->
            begin
                match op with
                    | UMinus -> EUop(UMinus, (differentiate e1 x))
                    | UExp -> EBop(BMul, (differentiate e1 x), EUop(UExp, e1))
                    | ULog -> EBop(BDiv, (differentiate e1 x), e1)
                    | UCos -> EBop(BMul, (differentiate e1 x), EUop(UMinus, EUop(USin, e1)))
                    | USin -> EBop(BMul, (differentiate e1 x), EUop(UCos, e1))
                    | UTan -> EBop(BDiv, (differentiate e1 x), EBop(BMul, EUop(UCos, e1),EUop(UCos, e1)))
                    | UAcos -> EUop(UMinus, EBop(BDiv, (differentiate e1 x), EBop(BPow, EBop(BSub, EInt(1), EBop(BMul, e1, e1)), EFloat(0.5))))
                    | UAsin -> EBop(BDiv, (differentiate e1 x), EBop(BPow, EBop(BSub, EInt(1), EBop(BMul, e1, e1)), EFloat(0.5)))
                    | UAtan -> EBop(BDiv, (differentiate e1 x), EBop(BAdd, EInt(1), EBop(BMul, e1, e1)))
                    | UCosh -> EBop(BMul, (differentiate e1 x), EUop(USinh, e1))
                    | USinh -> EBop(BMul, (differentiate e1 x), EUop(UCosh, e1))
                    | UTanh -> EBop(BDiv, (differentiate e1 x), EBop(BMul, EUop(UCosh, e1),EUop(UCosh, e1)))
                    | UCeil -> failwith "ERREUR : Dérivation de fonction non dérivable"
                    | UFloor -> failwith "ERREUR : Dérivation de fonction non dérivable"
                    | URound -> failwith "ERREUR : Dérivation de fonction non dérivable"
            end
        | EFloat(_) -> EInt(0)
        | EInt(_) -> EInt(0)
        | EVar(y) when y = x -> EInt(1)
        | EVar(y) -> EInt(0)