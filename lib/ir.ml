open Result
open Containers

type value = 
    | Literal of int64
    | Variable of string

type ir_expr =
    | Add of value * value
    | Sub of value * value
    | Div of value * value
    | Mul of value * value
    | FunctionCall of string * value list
    | Value of value

type statement =
    | Print of value
    | Set of string * ir_expr
    | If of value * block * block

and block = statement list * value

type top_level =
    | DefFunction of (string list) * block
    | BasicStatement of statement

type ir_module = top_level list * value

module VarSet = CCSet.Make(String)

type defined_function = {
    arity: int
}

module DefinedFunctionMap = CCMap.Make(String)

module Context = struct
    type correct_context = {
        variables: VarSet.t;
        functions: defined_function DefinedFunctionMap.t;
        statements: statement list;
        top_levels: top_level list;
        counter: int;
    }
    type t = 
        | Correct of correct_context
        | Err of string
    let empty = Correct {
        variables = VarSet.empty;
        functions = DefinedFunctionMap.empty;
        statements = List.empty;
        top_levels = List.empty;
        counter = 0;
    }
    let with_error _ctx error = Err error
    let map apply ctx =
        match ctx with
            | Correct ctx -> Correct (apply ctx)
            | Err e -> Err e
    let bind apply ctx =
        match ctx with
            | Correct ctx -> apply ctx
            | Err e -> Err e
    let from_result res = 
        match res with
            | Ok ctx -> ctx
            | Error e -> Err e
    let add_statement ctx statement = map (fun ctx -> { ctx with statements = ctx.statements @ [statement] }) ctx
    let add_top_level ctx top_level = map (fun ctx -> { ctx with top_levels = ctx.top_levels @ [top_level] }) ctx
    let add_variable ctx name = map (fun ctx -> { ctx with variables = VarSet.add name ctx.variables }) ctx
    let add_function ctx name func = map (fun ctx -> { ctx with functions = DefinedFunctionMap.add name func ctx.functions }) ctx
    let to_result ctx = match ctx with
                        | Correct ctx -> Ok ctx
                        | Err e -> Error e
    let lookup_variable ctx name = match ctx with
                                    | Correct ctx -> (match VarSet.mem name ctx.variables with
                                                        | true -> Ok name
                                                        | false -> Error ("Variable " ^ name ^ " does not exist"))
                                    | Err e -> Error e
    let create_child_context ctx = map (fun ctx -> { ctx with top_levels = List.empty; statements = List.empty }) ctx
    let get_statements ctx = match ctx with
                                | Correct corr -> corr.statements
                                | Err _e -> []
    let lookup_function ctx funcname = match ctx with
                                        | Correct ctx -> (match DefinedFunctionMap.find_opt funcname ctx.functions with
                                                            | None -> Error ("Function " ^ funcname ^ " not found")
                                                            | Some func -> Ok func)
                                        | Err e -> Error e
    let next_temp ctx = match ctx with
                            | Correct ctx -> 
                                let c = ctx.counter in 
                                Some ("temp" ^ (Int.to_string c)), Correct { ctx with counter = c + 1 }
                            | Err _ -> None, ctx
end