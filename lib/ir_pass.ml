open Ast
open Ir
open Result

let lower_value expr =
    match expr with
        | Atom (Identifier id) -> Ok (Variable id) (*TODO: Check for variable existence*)
        | Atom (IntLiteral il) -> Ok (Literal (Int64.of_string il))
        | _ -> Error ("Expected value/atom, got: " ^ (to_string expr))

let lower_basic_expression expr statements = 
    Ok (lower_value expr |> Result.get_ok, statements) (*TODO: Support nested expressions*)

let rec lower_block lower_statement exprs state =
    Result.bind state (fun (top_levels, statements) ->
        match exprs with
                | [] -> Error "Empty program"
                | [last] -> lower_basic_expression last statements |> Result.map (fun (result, statements) -> let block = statements, result in Ok { top_levels; block })
                | head :: rest -> lower_statement head top_levels statements |> lower_block lower_statement rest)

let lower_basic_statement statement statements =
    match statement with
        | Tree [] -> Ok statements
        | Tree ((Atom (Identifier "def")) :: (Atom (Identifier name)) :: expr :: []) -> 
            lower_basic_expression expr statements |> Result.map (fun (value, statements) -> statements @ [ Set (name, value) ])
        | Tree ((Atom (Identifier "do")) :: _rest) -> failwith "TODO"
        | Atom _ -> Error ("Expected statement, got: " ^ (to_string statement))
        | _ -> Error "not implemented"

let lower_top_statement statement top_levels statements =
    match statement with
        | _ -> lower_basic_statement statement statements |> Result.map (fun sts -> (top_levels, sts))

let lower_program ast = 
    lower_block lower_top_statement ast (Ok ([], []))