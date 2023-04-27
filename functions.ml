open Ast

let add a b =
    match a, b with
        | VInt(x), VFloat(y) -> VFloat(float_of_int x +. y)
        | VInt(x), VInt(y) -> VInt(x + y)
        | VFloat(x), VFloat(y) -> VFloat(x +. y)
        | VFloat(x), VInt(y) -> VFloat(float_of_int y +. x)
;;

let sub a b =
    match a, b with
        | VInt(x), VFloat(y) -> VFloat(float_of_int x -. y)
        | VInt(x), VInt(y) -> VInt(x - y)
        | VFloat(x), VFloat(y) -> VFloat(x -. y)
        | VFloat(x), VInt(y) -> VFloat(float_of_int y -. x)
;;

let mult a b =
    match a, b with
        | VInt(x), VFloat(y) -> VFloat(float_of_int x *. y)
        | VInt(x), VInt(y) -> VInt(x * y)
        | VFloat(x), VFloat(y) -> VFloat(x *. y)
        | VFloat(x), VInt(y) -> VFloat(float_of_int y *. x)
;;

let div a b =
    match a, b with
        | _, VFloat(0.)
        | _, VInt(0) -> failwith "ERREUR : Division par 0"
        | x, VFloat(y) -> mult x (VFloat(1. /. y))
        | x, VInt(y) -> mult x (VFloat(1. /. (float_of_int y)))
;;

let minus a = mult (VInt(-1)) a;;

let exp a = VFloat(Float.exp (force_value_to_float a));;
let log a = VFloat(Float.log (force_value_to_float a));;
let cos a = VFloat(Float.cos (force_value_to_float a));;
let sin a = VFloat(Float.sin (force_value_to_float a));;
let tan a = VFloat(Float.tan (force_value_to_float a));;
let acos a = VFloat(Float.acos (force_value_to_float a));;
let asin a = VFloat(Float.asin (force_value_to_float a));;
let atan a = VFloat(Float.atan (force_value_to_float a));;
let cosh a = VFloat(Float.cosh (force_value_to_float a));;
let sinh a = VFloat(Float.sinh (force_value_to_float a));;
let tanh a = VFloat(Float.tanh (force_value_to_float a));;
let ceil a = VFloat(Float.ceil (force_value_to_float a));;
let floor a = VFloat(Float.floor (force_value_to_float a));;
let round a = VFloat(Float.round (force_value_to_float a));;
let trunc a = VFloat(Float.trunc (force_value_to_float a));;

let power a b =
  match a, b with
  | VInt(x), VInt(y) -> VInt(int_of_float((float_of_int x) ** (float_of_int y)))
  | VInt(x), VFloat(y) -> VFloat((float_of_int x) ** y)
  | VFloat(x), VInt(y) -> VFloat(x ** (float_of_int y))
  | VFloat(x), VFloat(y) -> VFloat(x ** y)
;;

(** Polynom part, polynom are represented by an int list with power = degree - indice on the list**)


let rec padding a n = a @ (List.init n (fun _ -> 0));;

let split_list lst i =
  let k = List.length lst - i in
  let rec aux acc n = function
    | [] -> (List.rev acc, [])
    | x::xs ->
        if n = 0 then (List.rev acc, x::xs)
        else aux (x::acc) (n-1) xs
  in
  if k <= 0 then ([], lst)
  else if k >= List.length lst then (lst, [])
  else aux [] k lst
;;

let p_plus a b =
    let rec sum_lists lst1 lst2 =
        match (lst1, lst2) with
            | ([], []) -> []
            | ([], ys) -> ys
            | (xs, []) -> xs
            | (x::xs, y::ys) -> (x + y)::(sum_lists xs ys)
    in
    List.rev (sum_lists (List.rev a) (List.rev b))
;;

let rec p_mult a b =
    match a, b with
        | [], _ -> []
        | _, [] -> []
        | [t], _ -> List.map (fun x -> x * t) b
        | t1::q1, _ -> p_plus (padding (List.map (fun x -> x * t1) b) (List.length a - 1)) (p_mult q1 b)
;;

let rec karatsuba f g =
    match f, g with
        | [], _ -> []
        | _, [] -> []
        | [t], _ -> List.map (fun x -> x * t) g
        | _, [t] -> List.map (fun x -> x * t) f
        | _, _ -> let k = (max (List.length f) (List.length g)) / 2 in
                  let f1, f0 = split_list f k in
                  let g1, g0 = split_list g k in
                  let b = padding (p_plus (karatsuba f1 g0) (karatsuba f0 g1)) k in
                  let a = karatsuba f0 g0 in
                  let c = padding (karatsuba f1 g1) (2 * k) in
                  p_plus a (p_plus b c)
;;