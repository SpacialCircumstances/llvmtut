open Ast
open Ir
open Result
open Containers

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
    }
    type t = 
        | Correct of correct_context
        | Err of string
    let empty = Correct {
        variables = VarSet.empty;
        functions = DefinedFunctionMap.empty;
        statements = List.empty;
        top_levels = List.empty;
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
end

let lower_value expr =
    match expr with
        | Atom (Identifier id) -> Ok (Variable id) (*TODO: Check for variable existence*)
        | Atom (IntLiteral il) -> Ok (Literal (Int64.of_string_exn il))
        | _ -> Error ("Expected value/atom, got: " ^ (to_string expr))

let lower_expression expr ctx = 
    match expr with
        | Atom _ -> lower_value expr, ctx
        | _ -> Error ("Expected expression, got: " ^ (to_string expr)), ctx

let rec lower_block lower_statement lower_expr exprs ctx =
    match exprs with
        | [] -> Error "Empty program", ctx
        | [last] -> lower_expr last ctx
        | head :: rest -> lower_statement head ctx |> lower_block lower_statement lower_expr rest

let lower_do_block lower_statement expr ctx =
    match expr with
        | Tree ((Atom (Identifier "do")) :: nodes) -> lower_block lower_statement lower_expression nodes ctx
        | _ -> lower_expression expr ctx

let lower_basic_statement statement ctx =
    match statement with
        | Tree [] -> ctx
        | Tree ((Atom (Identifier "def")) :: (Atom (Identifier name)) :: expr :: []) -> 
            let (value, ctx) = lower_expression expr ctx in
            let ctx = Context.add_variable ctx name in
            Result.map (fun value ->  Set (name, value) |> Context.add_statement ctx) value |> Context.from_result
        | Tree ((Atom (Identifier "if")) :: condition :: if_true :: if_false :: []) ->
            let (_l_cond, ctx) = lower_expression condition ctx in
            let (_l_if_true, _true_ctx) = lower_expression if_true ctx in
            let (_l_if_false, _false_ctx) = lower_expression if_false ctx in
            ctx (*TODO*)
            (*Result.(>>=) () (fun (l_cond, statements) ->
                Result.(>>=) (lower_do_block lower_basic_statement if_true statements) (fun l_if_true ->
                    Result.(>>=) (lower_do_block lower_basic_statement if_false statements) (fun l_if_false ->
                        Ok (statements @ [ If (l_cond, l_if_true, l_if_false) ])
                    )
                )
            )*)
        | Tree ((Atom (Identifier "print")) :: expr :: []) -> 
            let value, ctx = lower_expression expr ctx in
            Result.map (fun value -> Print value |> Context.add_statement ctx) value |> Context.from_result
        | Atom _ -> Context.with_error ctx ("Expected statement, got: " ^ (to_string statement))
        | _ -> Context.with_error ctx "not implemented"

let lower_top_statement statement ctx =
    match statement with
        | _ -> lower_basic_statement statement ctx

let lower_program ast = 
    let value, ctx = lower_block lower_top_statement (fun expr ctx -> lower_expression expr ctx) ast Context.empty in
    Result.(and+) value (Context.to_result ctx)