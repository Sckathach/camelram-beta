let () = print_endline "Tests de Sckathach sur la compilation";;

type formula =
      Value of int
    | Plus of formula * formula
    | Times of formula * formula;;

let plus a b = a + b;;

let times a b = a * b;;

let rec value = function
    | Value x -> x
    | Plus (d, g) -> plus (value d) (value g)
    | Times (d, g) -> times (value d) (value g)

let a = Plus(Value 4, Times(Plus(Value 2, Value 3), Value 4))
