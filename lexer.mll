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
                | "d" -> D
                | "\exp" -> EXP
                | "\log" -> LOG
                | "\cos" -> COS
                | "\sin" -> SIN
                | "\tan" -> TAN
                | "\acos" -> ACOS
                | "\asin" -> ASIN
                | "\atan" -> ATAN
                | "\cosh" -> COSH
                | "\sinh" -> SINH
                | "\tanh" -> TANH
                | "\ceil" -> CEIL
                | "\floor" -> FLOOR
                | "\round" -> ROUND
                | "\infty" -> INFTY
                | "\pi" -> PI
                | "\frac" -> FRAC
                | "\derive" -> DERIVE
                | "\pol" -> POL
                | "\pGcd" -> PGCD
                | "\pMult" -> PMULT
                | "\pAdd" -> PADD
                | "\pMultScal" -> PMULTSCAL
                | "\pDivQ" -> PDIVQ
                | "\pDivR" -> PDIVR
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
    | ['.']
        { DOT }
    | eof
        { EOF }
    | _ as c
        { raise (Error c) }
