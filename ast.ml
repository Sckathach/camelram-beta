open Printf

(**
    TYPE bop
    Binary Operator, used to simplify pattern matching
*)
type bop = BAdd | BSub | BMul | BDiv | BPow

(**
    TYPE uop
    Unary Operator, used to simplify pattern matching
*)
type uop =  UMinus | UExp | ULog | UCos | USin | UTan
            | UAcos | UAsin | UAtan | UCosh | USinh | UTanh
            | UCeil | UFloor | URound | UTrunc

(**
    TYPE expr
    Type of the Abstract Syntax Tree
*)
type expr =
    | EInt          of int
    | EFloat        of float
    | EBop          of bop * expr * expr
    | EUop          of uop * expr
    | EIntegral     of expr * expr * expr
    | EIntegralD    of expr * expr * expr * expr
    | EDifferentiate        of expr * expr
    | ELet          of string * expr * expr
    | EVar          of string
    | EFun          of func
and func = string * arg list * expr
and arg =
    | AVar      of string

(**
    TYPE value
    Used when the AST is evaluated
*)
type value =
    | VInt      of int
    | VFloat    of float

(**
    FUNCTION force_value_to_float
    @type val force_value_to_float : value -> float = <fun>
*)
let force_value_to_float = function
    | VInt(x) -> float_of_int x
    | VFloat(x) -> x


(**
    AFFICHAGE
*)
let rec string_of_expr = function
    | EInt(x) -> (Printf.sprintf "EInt(%d)" x)
    | EFloat(x) -> "EFloat(" ^ (string_of_float x) ^ ")"
    | EVar(x) -> (Printf.sprintf "EVar(%s)" x)
    | EBop(op, e1, e2) ->
        begin
            match op with
                | BAdd -> "EBop(BAdd, " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
                | BSub -> "EBop(BSub, " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
                | BMul -> "EBop(BMul, " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
                | BDiv -> "EBop(BDiv, " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
                | BPow -> "EBop(BPow, " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
        end
    | EUop(op, e1) ->
        begin
            match op with
                | UMinus -> "EUop(UMinus, " ^ (string_of_expr e1) ^ ")"
                | UExp -> "EUop(UExp, " ^ (string_of_expr e1) ^ ")"
                | ULog -> "EUop(ULog, " ^ (string_of_expr e1) ^ ")"
                | UCos -> "EUop(UCos, " ^ (string_of_expr e1) ^ ")"
                | USin -> "EUop(USin, " ^ (string_of_expr e1) ^ ")"
                | UTan -> "EUop(UTan, " ^ (string_of_expr e1) ^ ")"
                | UAcos -> "EUop(UAcos, " ^ (string_of_expr e1) ^ ")"
                | UAsin -> "EUop(UAsin, " ^ (string_of_expr e1) ^ ")"
                | UAtan -> "EUop(UAtan, " ^ (string_of_expr e1) ^ ")"
                | UCosh -> "EUop(UCosh, " ^ (string_of_expr e1) ^ ")"
                | USinh -> "EUop(USinh, " ^ (string_of_expr e1) ^ ")"
                | UTanh -> "EUop(UTanh, " ^ (string_of_expr e1) ^ ")"
                | UCeil -> "EUop(UCeil, " ^ (string_of_expr e1) ^ ")"
                | UFloor -> "EUop(UFloor, " ^ (string_of_expr e1) ^ ")"
                | URound -> "EUop(URound, " ^ (string_of_expr e1) ^ ")"
                | UTrunc -> "EUop(UTrunc, " ^ (string_of_expr e1) ^ ")"
        end
    | EIntegral(expr, bb, bh) -> "EIntegral(" ^ (string_of_expr expr) ^ ", " ^ (string_of_expr bb) ^ ", " ^ (string_of_expr bh) ^ ")"
    | EIntegralD(expr, bb, bh, d) -> "EIntegralD(" ^ (string_of_expr expr) ^ ", " ^ (string_of_expr bb) ^ ", " ^ (string_of_expr bh) ^ ", " ^ (string_of_expr d) ^ ")"
    | EDifferentiate(expr, x) -> "EDifferentiate(" ^ (string_of_expr expr) ^ ", " ^ (string_of_expr x) ^ ")"
    | ELet(x, e1, e2) -> "ELet(" ^ x ^ ", " ^ (string_of_expr e1) ^ ", " ^ (string_of_expr e2) ^ ")"
    (*| EFun*)

let rec latex_of_expr = function
    | EInt(x) -> (Printf.sprintf "%d" x)
    | EFloat(x) -> (string_of_float x)
    | EVar(x) -> (Printf.sprintf "%s" x)
    | EBop(op, e1, e2) ->
        begin
            match op with
                | BAdd -> "(" ^ (string_of_expr e1) ^ " + " ^ (string_of_expr e2) ^ ")"
                | BSub -> "(" ^ (string_of_expr e1) ^ " - " ^ (string_of_expr e2) ^ ")"
                | BMul -> (string_of_expr e1) ^ " * " ^ (string_of_expr e2)
                | BDiv -> "\\frac{" ^ (string_of_expr e1) ^ "}{" ^ (string_of_expr e2) ^ "}"
                | BPow -> (string_of_expr e1) ^ "^{" ^ (string_of_expr e2) ^ "}"
        end
    | EUop(op, e1) ->
        begin
            match op with
                | UMinus -> "-(" ^ (string_of_expr e1) ^ ")"
                | UExp -> "\\exp{" ^ (string_of_expr e1) ^ "}"
                | ULog -> "\\log{" ^ (string_of_expr e1) ^ "}"
                | UCos -> "\\cos{" ^ (string_of_expr e1) ^ "}"
                | USin -> "\\sin{" ^ (string_of_expr e1) ^ "}"
                | UTan -> "\\tan{" ^ (string_of_expr e1) ^ "}"
                | UAcos -> "\\arccos{" ^ (string_of_expr e1) ^ "}"
                | UAsin -> "\\arcsin{" ^ (string_of_expr e1) ^ "}"
                | UAtan -> "\\arctan{" ^ (string_of_expr e1) ^ "}"
                | UCosh -> "\\cosh{" ^ (string_of_expr e1) ^ "}"
                | USinh -> "\\sinh{" ^ (string_of_expr e1) ^ "}"
                | UTanh -> "\\tanh{" ^ (string_of_expr e1) ^ "}"
                | UCeil -> "\\lceil " ^ (string_of_expr e1) ^ " \\rceil"
                | UFloor -> "\\lfloor " ^ (string_of_expr e1) ^ " \\rfloor"
                | URound -> "\\lfloor " ^ (string_of_expr e1) ^ " \\rceil"
                | UTrunc -> "\\trunc{" ^ (string_of_expr e1) ^ "}"
        end
    | EIntegral(expr, bb, bh) ->
        "\\int_{" ^ (string_of_expr bb) ^ "}^{" ^ (string_of_expr bh) ^ "} " ^ (string_of_expr expr) ^ "\\,d" (*PROBLEME*)
    | EIntegralD(expr, bb, bh, d) ->
        "\\int_{" ^ (string_of_expr bb) ^ "}^{" ^ (string_of_expr bh) ^ "} " ^ (string_of_expr expr) ^ "\\,d" ^ (string_of_expr d)
    | EDifferentiate(expr, x) ->
        "\\frac{\\mathrm{d}}{\\mathrm{d}" ^ (string_of_expr x) ^ "}(" ^ (string_of_expr expr) ^ ")"
    | ELet(x, e1, e2) ->
        "let " ^ x ^ " = " ^ (string_of_expr e1) ^ " in " ^ (string_of_expr e2)
    (*| EFun*)