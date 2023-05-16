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
            | UCeil | UFloor | URound

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
    | EPol          of string * expr
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
    FUNCTION pprint_value
    @type val pprint_value : value -> string = <fun>
*)
let pprint_value = function
    | VFloat(x) -> Printf.sprintf "%f" x
    | VInt(x) -> Printf.sprintf "%d" x

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
                | BAdd -> "(" ^ (latex_of_expr e1) ^ " + " ^ (latex_of_expr e2) ^ ")"
                | BSub -> "(" ^ (latex_of_expr e1) ^ " - " ^ (latex_of_expr e2) ^ ")"
                | BMul -> (latex_of_expr e1) ^ " * " ^ (latex_of_expr e2)
                | BDiv -> "\\frac{" ^ (latex_of_expr e1) ^ "}{" ^ (latex_of_expr e2) ^ "}"
                | BPow -> (latex_of_expr e1) ^ "^{" ^ (latex_of_expr e2) ^ "}"
        end
    | EUop(op, e1) ->
        begin
            match op with
                | UMinus -> "-(" ^ (latex_of_expr e1) ^ ")"
                | UExp -> "\\exp(" ^ (latex_of_expr e1) ^ ")"
                | ULog -> "\\log(" ^ (latex_of_expr e1) ^ ")"
                | UCos -> "\\cos(" ^ (latex_of_expr e1) ^ ")"
                | USin -> "\\sin(" ^ (latex_of_expr e1) ^ ")"
                | UTan -> "\\tan(" ^ (latex_of_expr e1) ^ ")"
                | UAcos -> "\\arccos{" ^ (latex_of_expr e1) ^ "}"
                | UAsin -> "\\arcsin{" ^ (latex_of_expr e1) ^ "}"
                | UAtan -> "\\arctan{" ^ (latex_of_expr e1) ^ "}"
                | UCosh -> "\\cosh{" ^ (latex_of_expr e1) ^ "}"
                | USinh -> "\\sinh{" ^ (latex_of_expr e1) ^ "}"
                | UTanh -> "\\tanh{" ^ (latex_of_expr e1) ^ "}"
                | UCeil -> "\\lceil " ^ (latex_of_expr e1) ^ " \\rceil"
                | UFloor -> "\\lfloor " ^ (latex_of_expr e1) ^ " \\rfloor"
                | URound -> "\\lfloor " ^ (latex_of_expr e1) ^ " \\rceil"
        end
    | EIntegral(expr, bb, bh) ->
        "\\int_{" ^ (latex_of_expr bb) ^ "}^{" ^ (latex_of_expr bh) ^ "}(" ^ (latex_of_expr expr) ^ ")"
    | EIntegralD(expr, bb, bh, d) ->
        "\\int_{" ^ (latex_of_expr bb) ^ "}^{" ^ (latex_of_expr bh) ^ "} " ^ (latex_of_expr expr) ^ "\\,d" ^ (latex_of_expr d)
    | EDifferentiate(expr, x) ->
        "\\frac{\\mathrm{d}}{\\mathrm{d}" ^ (latex_of_expr x) ^ "}(" ^ (latex_of_expr expr) ^ ")"
    | ELet(x, e1, e2) ->
        "let " ^ x ^ " = " ^ (latex_of_expr e1) ^ " in " ^ (latex_of_expr e2)