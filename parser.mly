%{
   open Ast
%}

%token <int> INT
%token EQUAL "="
%token ADD "+"
%token SUB "-"
%token MUL "*"
%token DIV "/"
%token POW "^"
%token LPAREN "("
%token RPAREN ")"
%token LCBRA "{"
%token RCBRA "}"
%token LET
%token IN
%token INTEGRAL
%token D "d"
%token <string> IDENT
%token EOF
%token EXP
%token LOG
%token COS
%token SIN
%token TAN
%token ACOS
%token ASIN
%token ATAN
%token COSH
%token SINH
%token TANH
%token CEIL
%token FLOOR
%token ROUND
%token TRUNC
%token INFTY
%token PI

%left "+" "-"
%left "*" "/"
%left "^"
%nonassoc UMINUS, EXP, LOG, COS, SIN, TAN, ACOS, ASIN, ATAN, COSH, SINH, TANH, CEIL, FLOOR, ROUND, TRUNC

%start main
%type <expr> main

%%

main:
    | e = let_expr EOF
        { e }

let_expr:
    | LET id = IDENT "=" e1 = let_expr IN e2 = let_expr
        { ELet(id, e1, e2) }
    | e = expr
        { e }

expr:
    | i = INT
        { EInt(i) }
    | INTEGRAL e1 = expr "^" e2 = expr e3 = expr "d" e4 = expr
        { EIntegralD(e1, e2, e3, e4) }
    | INTEGRAL e1 = expr "^" e2 = expr e3 = expr
        { EIntegral(e1, e2, e3) }
    | e1 = expr "+" e2 = expr
        { EBop(BAdd, e1, e2) }
    | e1 = expr "-" e2 = expr
        { EBop(BSub, e1, e2) }
    | e1 = expr "*" e2 = expr
        { EBop(BMul, e1, e2) }
    | e1 = expr "/" e2 = expr
        { EBop(BDiv, e1, e2) }
    | e1 = expr "^" e2 = expr
        { EBop(BPow, e1, e2) }
    | "-" e = expr %prec UMINUS
        { EUop(UMinus, e) }
    | EXP e = expr %prec EXP
        { EUop(UExp, e) }
    | LOG e = expr %prec LOG
        { EUop(ULog, e) }
    | COS e = expr %prec COS
        { EUop(UCos, e) }
    | SIN e = expr %prec SIN
        { EUop(USin, e) }
    | TAN e = expr %prec TAN
        { EUop(UTan, e) }
    | ACOS e = expr %prec ACOS
        { EUop(UAcos, e) }
    | ASIN e = expr %prec ASIN
        { EUop(UAsin, e) }
   | ATAN e = expr %prec ATAN
        { EUop(UAtan, e) }
    | COSH e = expr %prec COSH
        { EUop(UCosh, e) }
    | SINH e = expr %prec SINH
        { EUop(USinh, e) }
    | TANH e = expr %prec TANH
        { EUop(UTanh, e) }
    | CEIL e = expr %prec CEIL
        { EUop(UCeil, e) }
    | FLOOR e = expr %prec FLOOR
        { EUop(UFloor, e) }
    | ROUND e = expr %prec ROUND
        { EUop(URound, e) }
    | TRUNC e = expr %prec TRUNC
        { EUop(UTrunc, e) }
    | x = IDENT
        { EVar(x) }
    | x = INFTY
        { EVar("infty") }
    | x = PI
        { EVar("pi") }
    | "(" e = let_expr ")"
        { e }
    | "{" e = let_expr "}"
        { e }
