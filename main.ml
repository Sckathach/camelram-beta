open Printf
open Ast
open Calc

(**
 /\_/\
( o.o )
 > ^ <

 To protect the code from bugs
*)

(**
    FUNCTION main
    @requires File in stdin
    @type val main : unit -> unit = <fun>
    @returns Evaluation of the LaTeX string in stdin
*)
let main =
    let lexbuf = Lexing.from_channel stdin in
    let res =
        try Parser.main Lexer.token lexbuf
        with
        (* Try to catch some errors, use `menhir parser.mly  --explain` for more information *)
        | Lexer.Error c ->
            fprintf stderr "Lexical error at line %d: Unknown character '%c'\n" lexbuf.lex_curr_p.pos_lnum c;
            exit 1
        | Parser.Error ->
            fprintf stderr "Parse error at line %d:\n" lexbuf.lex_curr_p.pos_lnum;
            exit 1
    in
        (* Printf.printf "%s\n" (pprint_expr res) *)
        Printf.printf "%s\n" (pprint_value (eval res))
