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
%token <string> IDENT
%token EOF

%left "+" "-"
%left "*" "/"
%left "^"
%nonassoc UMINUS

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
    | x = IDENT
        { EVar(x) }
    | "(" e = let_expr ")"
        { e }
    | "{" e = let_expr "}"
        { e }
