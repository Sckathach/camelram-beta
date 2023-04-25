module Variable = struct
    open Map
    open Ast

    module Dict = Map.Make(String)

    let variables = ref Dict.empty;;
    variables := Dict.add "pi" (VFloat(3.14)) !variables

    let to_string = function
        | None -> "La variable n'existe pas."
        | Some(s) -> match s with
            | VInt(x) -> string_of_int x
            | VFloat(x) -> string_of_float x

    let get key =
        try Some(Dict.find key !variables) with
            Not_found -> None

    let add_float name value =
        variables := Dict.add name (VFloat(value)) !variables
    let add_int name value =
        variables := Dict.add name (VInt(value)) !variables
end