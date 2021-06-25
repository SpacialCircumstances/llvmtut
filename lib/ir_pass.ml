open Ir
open Result

let lower_expr _expr statements =
    statements

let lower_top_expr _expr top_levels statements = 
    top_levels, statements

let lower_to_ir ast = 
    let top_levels, statements = lower_top_expr ast [] [] in
    Ok { top_levels; statements }