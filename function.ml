module Function = struct
    open Map
    open Ast

    module DictFunc = Map.Make(String)

    let functions = ref DictFunc.empty

    let add name args expr =
        functions := DictFunc.add name (EFun(name, args, expr)) !functions
    let get name =
        try Some(DictFunc.find name !functions) with
            Not_found -> None
end