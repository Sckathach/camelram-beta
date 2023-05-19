(* module type Polynomial = sig *)
(*    exception BadType *)
(*    val add : Ast.expr -> Ast.expr -> Ast.expr *)
(* end *)
(*  *)
(* module Polynomial : Polynomial = struct *)
module Polynomial = struct
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

    let rec degree p =
        match p with
            | [] -> -1
            | (_, d) :: t -> d
    ;;

    let rec split p k =
    match p with
        | [] -> [], []
        | (a, b) :: q ->let t, g = split q k in
                        if b >= k then (a, b - k) :: t, g
                        else t, (a, b) :: g
    ;;

    let rec simple_mult p k =
        match p with
            | [] -> []
            | (a, b) :: q -> (a, b + k) :: (simple_mult q k)
    ;;

    let rec karatsuba f g =
        match f, g with
            | [], _ -> []
            | _, [] -> []
            | [(a, k)], _ -> simple_mult (mult_scal_poly a g) k
            | _, [(a, k)] -> simple_mult (mult_scal_poly a f) k
            | _, _ -> let k = max ((max (degree f) (degree g)) / 2) 1 in
                  let f1, f0 = split f k in
                  let g1, g0 = split g k in
                  let b = simple_mult (add_poly (karatsuba f1 g0) (karatsuba f0 g1)) k in
                  let a = karatsuba f0 g0 in
                  let c = simple_mult (karatsuba f1 g1) (2 * k) in
                  add_poly a (add_poly b c)
    ;;

(*    let rec mult_poly p q = *)
(*        List.rev (karatsuba (List.rev p) (List.rev q)) *)
(*    ;; *)

    let rec leading_term p =
        match p with
            | [] -> VInt(0)
            | (a, b) :: q -> a
    ;;

    let rev l = List.rev l ;;

    let rec div_poly p q =
        if degree p < degree q then [], p
        else
            let quotient = General.div (leading_term p) (leading_term q) in
            let quotient_degree = (degree p) - (degree q) in
            let quotient_poly = (quotient, quotient_degree) in
            let remainder = add_poly p (mult_scal_poly (VInt(-1)) (karatsuba q [quotient_poly])) in
            let recursiveQuotient, recursiveRemainder = div_poly remainder q in
            (quotient_poly :: recursiveQuotient), recursiveRemainder
    ;;
    let a = rev [(VInt(1), 0); (VInt(2), 1); (VInt(3), 2); (VInt(4), 3)];;
    let b = rev [(VInt(1), 0); (VInt(2), 1)];;
    let c = rev [(VFloat(0.2), 1); (VFloat(1.5), 2)];;

    let rec gcd_poly p q =
        if degree q > degree p then gcd_poly q p
        else if is_zero q then p
            else
                let _, r = div_poly p q in
                gcd_poly q r
    ;;

    let normalize_poly p =
        match p with
            | [] -> []
            | (a, b) :: q -> mult_scal_poly (General.div (VInt(1)) a) p
    ;;

    let normalize_gcd_poly p q = normalize_poly (gcd_poly p q);;
end
