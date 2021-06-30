open Containers
open Result

let read_file filename =
    let ic = open_in filename in
    let text = IO.read_all ic in
    close_in ic;
    text

let compile filename = 
    let text = read_file filename in
    let ast = Parser.parse text in
    List.iter (fun e -> Ast.to_string e |> print_endline) ast;
    match Ir_pass.lower_program ast with
        | Ok _ir -> ()
        | Error errs -> List.iter print_endline errs