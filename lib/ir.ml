open Result
open Containers

type value = 
    | Literal of int64
    | Variable of string
    | ErrValue

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

type ir_module = {
    statements: statement list;
    top_levels: top_level list;
    retval: value;
}

module VarSet = CCSet.Make(String)

type defined_function = {
    arity: int
}

module DefinedFunctionMap = CCMap.Make(String)

let builtin_functions = DefinedFunctionMap.of_list [
    "+", { arity = 2 };
    "-", { arity = 2 };
    "*", { arity = 2 };
    "/", { arity = 2 }
]

module Context = struct
    type t = {
        variables: VarSet.t;
        functions: defined_function DefinedFunctionMap.t;
        statements: statement list;
        top_levels: top_level list;
        counter: int;
        errors: string list;
    }
    let empty = {
        variables = VarSet.empty;
        functions = builtin_functions;
        statements = List.empty;
        top_levels = List.empty;
        counter = 0;
        errors = List.empty;
    }
    let with_error ctx error = { ctx with errors = error :: ctx.errors }
    let add_statement ctx statement = { ctx with statements = ctx.statements @ [statement] }
    let add_top_level ctx top_level = { ctx with top_levels = ctx.top_levels @ [top_level] }
    let add_variable ctx name = { ctx with variables = VarSet.add name ctx.variables }
    let add_function ctx name func = { ctx with functions = DefinedFunctionMap.add name func ctx.functions }
    let to_result ctx value = match ctx.errors with
                        | [] -> Ok {
                            statements = ctx.statements;
                            top_levels = ctx.top_levels;
                            retval = value
                        }
                        | errors -> Error errors
    let lookup_variable ctx name = match VarSet.mem name ctx.variables with
                                    | true -> Ok (), ctx
                                    | false -> let e = "Variable " ^ name ^ " does not exist" in Error e, with_error ctx e
    let create_child_context ctx = { ctx with top_levels = List.empty; statements = List.empty }
    let integrate_child_context_errors child ctx = { ctx with errors = ctx.errors @ child.errors }
    let get_statements ctx = ctx.statements
    let lookup_function ctx funcname = match DefinedFunctionMap.find_opt funcname ctx.functions with
                                                            | None -> let e = "Function " ^ funcname ^ " not found" in Error e, with_error ctx e
                                                            | Some func -> (Ok func), ctx
    let next_temp ctx = let c = ctx.counter in 
                            "temp" ^ (Int.to_string c), { ctx with counter = c + 1 }
end