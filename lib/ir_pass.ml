open Ir
open Result

let lower_basic_expr _expr statements =
    statements

let lower_top_expr _expr _top_levels _statements =
    Error "unimplemented"

let lower_expr last_state expr =
    match last_state with
        | Ok (tl, s) -> lower_top_expr expr tl s
        | Error e -> Error e

let lower_program ast = 
    match List.fold_left lower_expr (Ok ([], [])) ast with
        | Ok (top_levels, statements) -> Ok { top_levels; statements }
        | Error e -> Error e