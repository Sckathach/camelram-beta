type value =
        | VInt of int
        | VFloat of float

let add a b =
    match a, b with
        | VInt(x), VFloat(y) -> VFloat(float_of_int x +. y)
        | VInt(x), VInt(y) -> VInt(x + y)
        | VFloat(x), VFloat(y) -> VFloat(x +. y)
        | VFloat(x), VInt(y) -> VFloat(float_of_int y +. x)
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








(** Fonction pow_int
    @returns a^b
    @type val pow_int : int -> int -> int = <fun>
*)
let rec pow_int (a : int) (b : int) =
    match b mod 2, b with
        | _, 0 -> 1
        | _, 1 -> a
        | 0, _ -> pow_int a (b / 2) * pow_int a (b / 2)
        | 1, _ -> a * pow_int a (b - 1)
;;