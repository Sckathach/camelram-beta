open Ast
open Printf
open Calc
open Function
open General
open Variable;;

let test_message x y m i =
    if x = y then
        (Printf.sprintf "TEST %i : Pass" i, true)
    else
        (Printf.sprintf "TEST %i : Fail %s" i m, false);;

let test x y i = test_message x y " " i;;

let tests = [
    test (vars (EBop(BAdd, EVar "x", EInt 3))) ["x"];
    test 3 2;
    test (floor (VFloat 3.14)) (VFloat 3.);
    test_message 3 4 "C loupé";
    test_message 3 3 "Ce message ne s'affichera pas uwWU"
]

let () =
    let rec aux b i = function
        [] ->
            if b then
                print_endline "*** FINISHED : OK ***"
            else
                print_endline "*** FINISHED : FAIL ***"
        | x :: q ->
            begin
                match x i with
                    y, z -> print_endline y; aux (z && b) (i + 1) q
            end
    in
        print_endline "*** START ***";
        aux true 0 tests;;
