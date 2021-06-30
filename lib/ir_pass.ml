open Ast
open Ir
open Result
open Containers

let swap (a, b) = (b, a)

let lower_value expr ctx =
    match expr with
        | Atom (Identifier id) -> let res, ctx = Context.lookup_variable ctx id in Result.map (fun () -> Variable id) res, ctx
        | Atom (IntLiteral il) -> Ok (Literal (Int64.of_string_exn il)), ctx
        | _ -> let err = "Expected value/atom, got: " ^ (to_string expr) in Error err, (Context.with_error ctx err)

let function_call funcname args lower_expr ctx =
    match Context.lookup_function ctx funcname with
        | Ok func, ctx -> if func.arity = List.length args then
                        let ctx, args_values = List.fold_map (fun ctx arg -> lower_expr arg ctx |> swap) ctx args in
                        Ok (FunctionCall (funcname, args_values)), ctx
                    else let e = "Function " ^ funcname ^ " has " ^ (Int.to_string func.arity) ^ " arguments, but got " ^ (List.length args |> Int.to_string) in Error e, Context.with_error ctx e
        | Error e, ctx -> Error e, ctx

let rec lower_expression expr ctx = 
    match expr with
        | Atom _ -> let value, ctx = lower_value expr ctx in Result.map (fun v -> Value v) value, ctx
        | Tree ((Atom (Identifier funcname)) :: args) -> function_call funcname args lower_expr_to_value ctx
        | _ -> Error ("Expected expression, got: " ^ (to_string expr)), ctx

and lower_set name expr ctx =
    let (expr, ctx) = lower_expression expr ctx in
        match expr with
            | Ok expr ->
                let ctx = Context.add_variable ctx name in
                Context.add_statement ctx (Set (name, expr))
            | Error e -> Context.with_error ctx e

and lower_expr_to_value expr ctx = let name, ctx = Context.next_temp ctx in Variable name, lower_set name expr ctx

let rec lower_block lower_statement lower_expr exprs ctx =
    match exprs with
        | [] -> ErrValue, Context.with_error ctx "Empty program"
        | [last] -> lower_expr_to_value last ctx
        | head :: rest -> lower_statement head ctx |> lower_block lower_statement lower_expr rest

let lower_do_block lower_statement expr ctx =
    match expr with
        | Tree ((Atom (Identifier "do")) :: nodes) -> lower_block lower_statement lower_expression nodes ctx
        | _ -> lower_expr_to_value expr ctx

let rec lower_basic_statement statement ctx =
    match statement with
        | Tree [] -> ctx
        | Tree ((Atom (Identifier "def")) :: (Atom (Identifier name)) :: expr :: []) -> 
            lower_set name expr ctx
        | Tree ((Atom (Identifier "if")) :: condition :: if_true :: if_false :: []) ->
            let (l_cond, ctx) = lower_expr_to_value condition ctx in
            let (l_if_true, true_ctx) = lower_do_block lower_basic_statement if_true (Context.create_child_context ctx) in
            let (l_if_false, false_ctx) = lower_do_block lower_basic_statement if_false (Context.create_child_context ctx) in
            let st = If (l_cond, (Context.get_statements true_ctx, l_if_true), (Context.get_statements false_ctx, l_if_false)) in
            let ctx = ctx |> Context.integrate_child_context_errors true_ctx |> Context.integrate_child_context_errors false_ctx in 
            Context.add_statement ctx st
        | Tree ((Atom (Identifier "print")) :: expr :: []) -> 
            let value, ctx = lower_expr_to_value expr ctx in
            Print value |> Context.add_statement ctx
        | Atom _ -> Context.with_error ctx ("Expected statement, got: " ^ (to_string statement))
        | _ -> Context.with_error ctx "not implemented"

let get_parameters params_nodes ctx =
    let folder ctx node =
        match node with
            | Atom (Identifier param_name) -> (ctx, Some param_name)
            | _ -> Context.with_error ctx ("Unexpected expression " ^ (to_string node) ^ " in function definition parameters"), None in
    let ctx, param_names = List.fold_map folder ctx params_nodes in
    List.filter_map (fun a -> a) param_names, ctx

let create_function_context funcname param_names ctx =
    let fctx = Context.create_child_context ctx in
    let fctx = Context.add_function fctx funcname { arity = List.length param_names } in
    List.fold_left (fun fctx param -> Context.add_variable fctx param) fctx param_names

let lower_top_statement statement ctx =
    match statement with
        | Tree ((Atom (Identifier "defn")) :: (Atom (Identifier funcname)) :: (Tree params) :: body :: []) -> 
            let param_names, ctx = get_parameters params ctx in
            let body_ctx = create_function_context funcname param_names ctx in
            let value, body_ctx = lower_do_block lower_basic_statement body body_ctx in
            let func_block = Context.get_statements body_ctx, value in
            Context.add_top_level ctx (DefFunction (param_names, func_block))
        | _ -> lower_basic_statement statement ctx

let lower_program ast = 
    (* TODO *)
    let _value, ctx = lower_block lower_top_statement (fun expr ctx -> lower_expression expr ctx) ast Context.empty in
    Context.to_result ctx