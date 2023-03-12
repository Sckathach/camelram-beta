let fact n =
    let rec aux acc = function
        0 -> acc
        | x -> aux (acc*x) (x-1)
    in aux 1 n
