{
    open Parser
    exception Error of char
}

let letter = ['A'-'Z'] | ['a'-'z']
let digit = ['0'-'9']
let non_digit = ['_' '\\'] | letter
let ident = non_digit+

let line_comment = "//" [^ '\n']*

rule token = parse
    |  [' ' '\t'] | line_comment
        { token lexbuf }
    | ['\n']
        { Lexing.new_line lexbuf; token lexbuf }
    | ident as str
        {
            match str with
                | "\int_" -> INTEGRAL
                | "let" -> LET
                | "in" -> IN
                | s -> IDENT(s)
        }
    | ['=']
        { EQUAL }
    | ['+']
        { ADD }
    | ['-']
        { SUB }
    | ['*']
        { MUL }
    | ['/']
        { DIV }
    | ['^']
        { POW }
    | digit+ as lxm
        { INT(int_of_string lxm) }
    | ['(']
        { LPAREN }
    | [')']
        { RPAREN }
    | ['{']
        { LCBRA }
    | ['}']
        { RCBRA }
    | eof
        { EOF }
    | _ as c
        { raise (Error c) }
