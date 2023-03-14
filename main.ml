open Printf
open Ast
open Calc

(* It just reads the file in stdin and returns the AST *)
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
        Printf.printf "%s\n" (pprint_expr res)
