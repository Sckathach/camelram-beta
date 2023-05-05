module Variable = struct
    open Map
    open Ast

    module Dict = Map.Make(String)

    let variables = ref Dict.empty;;
    variables := Dict.add "pi" (VFloat (Float.pi)) !variables;;
    variables := Dict.add "eps" (VFloat (Float.epsilon)) !variables;;
    variables := Dict.add "infty" (VFloat (Float.infinity)) !variables

    (**
        FUNCTION Variable.to_string
        @type val to_string : value -> string option = <fun>
        @returns Some x if the variable exists, None if it doesn't
    *)
    let to_string = function
        | None -> "La variable n'existe pas."
        | Some(s) -> match s with
            | VInt(x) -> string_of_int x
            | VFloat(x) -> string_of_float x

    (**
        FUNCTION Variable.get
        @type val get : string -> value option = <fun>
        @returns Some x if the variable exists, None if it doesn't
    *)
    let get key =
        try Some(Dict.find key !variables) with
            Not_found -> None

    let add_float name value =
        variables := Dict.add name (VFloat value) !variables
    let add_int name value =
        variables := Dict.add name (VInt value) !variables
    let add_e name value = match value with
        | EInt(x) -> variables := Dict.add name (VInt x) !variables
        | EFloat(x) -> variables := Dict.add name (VFloat x) !variables
    let add name value =
        variables := Dict.add name value !variables
end