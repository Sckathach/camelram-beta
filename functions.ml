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

let power a b =
  match a, b with
  | VInt(x), VInt(y) -> VInt(int_of_float((float_of_int x) ** (float_of_int y)))
  | VInt(x), VFloat(y) -> VFloat((float_of_int x) ** y)
  | VFloat(x), VInt(y) -> VFloat(x ** (float_of_int y))
  | VFloat(x), VFloat(y) -> VFloat(x ** y)
;;
