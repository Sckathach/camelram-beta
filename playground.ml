

let rec karatsuba a b =
    let n = max ((log (float_of_int a)) /. (log 2.)) ((log (float_of_int b)) /. (log 2.)) +. 1. in
    if n <= 1 then a * b
    else  



