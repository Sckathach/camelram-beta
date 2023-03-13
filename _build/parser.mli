
(* The type of tokens. *)

type token = 
  | SUB
  | RPAREN
  | MUL
  | LPAREN
  | LET
  | INT of (int)
  | IN
  | IDENT of (string)
  | EQUAL
  | EOF
  | DIV
  | ADD

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val main: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Ast.expr)
