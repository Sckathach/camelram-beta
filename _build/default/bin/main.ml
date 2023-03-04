let () = print_endline "Hello, World!";;

type formule =
      F of int
    | Ou of formule * formule
    | Et of formule * formule
    | Im of formule * formule
    | Non of formule;;

type valuation = int array;;

let ou a b =
  if a = 1 then a
  else b;;

let et a b =
  if (a = 1) && (b = 1) then 1
  else 0;;

let non a = 1 - a;;

let im a b = ou (non a) b;;

let xor a b = et (ou a b) (non (et a b));;

let a = Et(Et(F 0,Ou(F 1,F 2)),Non(Im(F 3,F 4)));;

let tab : valuation = [|1;1;0;1;1|];;



let rec evaluer a t = match a with
    F x -> t.(x)
  | Ou (d, g) -> ou (evaluer d t) (evaluer g t)
  | Et (d, g) -> et (evaluer d t) (evaluer g t)
  | Im (d, g) -> im (evaluer d t) (evaluer g t)
  | Non x -> non (evaluer x t);;

(*let evaluer_rev a t =*)
(*  let rec aux a t = match a with*)
(*      F x -> t.(n - x)*)
(*    | Ou (d, g) -> ou (evaluer_rev d t) (evaluer_rev g t)*)
(*    | Et (d, g) -> et (evaluer_rev d t) (evaluer_rev g t)*)
(*    | Im (d, g) -> im (evaluer_rev d t) (evaluer_rev g t)*)
(*    | Non x -> non (evaluer_rev x t);;*)

let rec ecrire1 a = match a with
  | F x -> string_of_int x
  | Ou (d, g) -> "(" ^ (ecrire1 d) ^ "v" ^ (ecrire1 g) ^ ")"
  | Et (d, g) -> "(" ^ (ecrire1 d) ^ "^" ^ (ecrire1 g) ^ ")"
  | Im (d, g) -> "(" ^ (ecrire1 d) ^ "#" ^ (ecrire1 g) ^ ")"
  | Non x -> "(-" ^ (ecrire1 x) ^ ")";;

let rec ecrire2 a t = match a with
  | F x -> string_of_int t.(x)
  | Ou (d, g) -> "(" ^ (ecrire2 d t) ^ "v" ^ (ecrire2 g t) ^ ")"
  | Et (d, g) -> "(" ^ (ecrire2 d t) ^ "^" ^ (ecrire2 g t) ^ ")"
  | Im (d, g) -> "(" ^ (ecrire2 d t) ^ "#" ^ (ecrire2 g t) ^ ")"
  | Non x -> "(-" ^ (ecrire2 x t) ^ ")";;

let string_to_liste s =
  let rec aux i l =
    if i > (String.length s - 1) then l
    else aux (i + 1) (s.[i] :: l)
  in aux 0 [];;


let creer_formule liste =
  let rec aux l foret op i = match l with
      [] ->
        let oui f = match f with
            [x] -> x
            | _ -> failwith "bonsoir"
        in oui foret
    | 'x'::q -> aux q ((F i)::foret) op (i + 1)
    | '('::q ->
        let creer f o = match f, o with
            x::q, '-'::p -> Non(x)::q, p
          | x::y::q, 'v'::p -> Ou(x,y)::q, p
          | x::y::q, '^'::p -> Et(x,y)::q, p
          | x::y::q, '#'::p -> Im(x,y)::q, p
        in
        let a, b = creer foret op in
          aux q a b i
    | ')'::q -> aux q foret op i
    | x::q -> aux q foret (x::op) i
  in aux liste [] [] 0;;
