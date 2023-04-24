
(** Fonction pow_int
    @returns a^b
    @type val pow_int : int -> int -> int = <fun>
*)
let rec pow_int (a : int) (b : int) =
    match b mod 2, b with
        | _, 0 -> 1
        | _, 1 -> a
        | 0, _ -> pow_int a (b / 2) * pow_int a (b / 2)
        | 1, _ -> a * pow_int a (b - 1);;


