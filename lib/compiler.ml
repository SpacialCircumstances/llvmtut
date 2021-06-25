open Containers

let read_file filename =
    let ic = open_in filename in
    let text = IO.read_all ic in
    close_in ic;
    text

let compile filename = 
    let text = read_file filename in
    let ast = Parser.parse text in
    Ast.to_string ast |> print_endline