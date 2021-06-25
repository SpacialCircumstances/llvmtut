open Ast
open Ir
open Result

let lower_value _expr =
    Ok (Literal Int64.zero)

let lower_basic_expr expr statements =
    match expr with
        | Tree [] -> Ok statements
        | _ -> Error "not implemented"

let lower_top_expr expr top_levels statements =
    match expr with
        | Atom _ -> Error ("Error: Atom " ^ (to_string expr) ^ " is not allowed as a top-level statement")
        | _ -> lower_basic_expr expr statements |> Result.map (fun sts -> (top_levels, sts))

let lower_expr last_state expr =
    match last_state with
        | Ok (tl, s) -> lower_top_expr expr tl s
        | Error e -> Error e

let lower_program ast = 
    let rec step exprs state =
        Result.bind state (fun (top_levels, statements) ->
            match exprs with
                    | [] -> Error "Empty program"
                    | [last] -> lower_value last |> Result.map(fun result -> let block = statements, result in Ok { top_levels; block })
                    | head :: rest -> lower_top_expr head top_levels statements |> step rest)
    in step ast (Ok ([], []))