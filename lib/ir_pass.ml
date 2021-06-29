open Ast
open Ir
open Result
open Containers

let lower_value expr ctx =
    match expr with
        | Atom (Identifier id) -> Result.map (fun name -> Variable name) (Context.lookup_variable ctx id), ctx
        | Atom (IntLiteral il) -> Ok (Literal (Int64.of_string_exn il)), ctx
        | _ -> let err = "Expected value/atom, got: " ^ (to_string expr) in Error err, (Context.with_error ctx err)

let lower_expression expr ctx = 
    match expr with
        | Atom _ -> lower_value expr ctx
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

let rec lower_basic_statement statement ctx =
    match statement with
        | Tree [] -> ctx
        | Tree ((Atom (Identifier "def")) :: (Atom (Identifier name)) :: expr :: []) -> 
            let (value, ctx) = lower_expression expr ctx in
            let ctx = Context.add_variable ctx name in
            Result.map (fun value ->  Set (name, value) |> Context.add_statement ctx) value |> Context.from_result
        | Tree ((Atom (Identifier "if")) :: condition :: if_true :: if_false :: []) ->
            let (l_cond, ctx) = lower_expression condition ctx in
            let (l_if_true, true_ctx) = lower_do_block lower_basic_statement if_true (Context.create_child_context ctx) in
            let (l_if_false, false_ctx) = lower_do_block lower_basic_statement if_false (Context.create_child_context ctx) in
            Result.map (fun (l_cond, (l_if_true, l_if_false)) ->
                let st = If (l_cond, (Context.get_statements true_ctx, l_if_true), (Context.get_statements false_ctx, l_if_false)) in
                Context.add_statement ctx st
            ) (Result.(and+) l_cond (Result.(and+) l_if_true l_if_false)) |> Context.from_result
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