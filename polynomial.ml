module type Polynomial = sig
    exception BadType
    val add : Ast.expr -> Ast.expr -> Ast.expr
    val simplify_frac : Ast.expr -> Ast.expr -> Ast.expr
    val mult : Ast.expr -> Ast.expr -> Ast.expr
end

module Polynomial : Polynomial = struct
    open Ast
    open General
    exception BadType

    type polynomial = (value * int) list

    let val_of_expr = function
        | EInt x -> VInt x
        | EFloat x -> VFloat x
        | _ -> raise BadType

    let expr_of_val = function
        | VInt x -> EInt x
        | VFloat x -> EFloat x

    let extract = function
        | EPol(x, p) -> x, p

    let pol_of_expr (variable : string) (expression : expr) : polynomial =
        let rec aux var acc = function
            | EBop(BAdd, EInt a, b) -> aux var ((VInt a, 0) :: acc) b
            | EBop(BAdd, EFloat a, b) -> aux var ((VFloat a, 0) :: acc) b
            | EBop(BMul, a, EVar var) -> (val_of_expr a, 1) :: acc
            | EBop(BMul, a, EBop(BPow, EVar var, EInt b)) -> (val_of_expr a, b) :: acc
            | EBop(BAdd, a, b) -> (aux var acc a) @ (aux var acc b)
            | _ -> raise BadType
        in aux variable [] expression

    let rec expr_of_pol var = function
        | [] -> EInt 0
        | [(a, b)] -> EBop(BMul, expr_of_val a, EBop(BPow, EVar var, EInt b))
        | x :: q -> EBop(BAdd, expr_of_pol var [x], expr_of_pol var q)

    let is_zero polynomial =
        List.for_all (fun (coeff, _) -> coeff = VInt 0 || coeff = VFloat 0.0) polynomial

    let simplify polynomial =
        List.filter (fun (coeff, _) -> coeff <> VInt 0 && coeff <> VFloat 0.0) polynomial

    let rec add_poly poly1 poly2 =
        match (poly1, poly2) with
        | [], _ -> poly2
        | _, [] -> poly1
        | (coeff1, exp1) :: rest1, (coeff2, exp2) :: rest2 ->
            if exp1 = exp2 then
                let sum = General.add coeff1 coeff2
            in
                if sum = VInt 0 || sum = VFloat 0.0 then
                    add_poly rest1 rest2
                else
                    (sum, exp1) :: add_poly rest1 rest2
                else if exp1 > exp2 then
                    (coeff1, exp1) :: add_poly rest1 poly2
                else
                    (coeff2, exp2) :: add_poly poly1 rest2

    let mult_scal_poly scalar poly =
        List.map (fun (coeff, exp) ->
                (General.mult coeff scalar, exp)
            ) poly

    let add p q =
        let vara, a = extract p in
        let varb, b = extract q in
        let c = pol_of_expr vara a in
        let d = pol_of_expr varb b in
        let e = add_poly c d in
        let f = expr_of_pol vara e in
        EPol(vara, f)
end
