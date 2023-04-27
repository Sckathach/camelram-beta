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
