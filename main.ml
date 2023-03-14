open Printf
open Ast
open Calc

let main =
    let lexbuf = Lexing.from_channel stdin in
    let res =
        try Parser.main Lexer.token lexbuf
        with
        | Lexer.Error c ->
            fprintf stderr "Lexical error at line %d: Unknown character '%c'\n" lexbuf.lex_curr_p.pos_lnum c;
            exit 1
        | Parser.Error ->
            fprintf stderr "Parse error at line %d:\n" lexbuf.lex_curr_p.pos_lnum;
            exit 1
    in
        let _ = res in
(*        Printf.printf "%d\n" (eval res);;*)
            Printf.printf "%s\n" (pprint_expr res)
