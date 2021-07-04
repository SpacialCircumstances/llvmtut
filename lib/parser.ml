open Angstrom
open Ast

let open_paren = char '('
let close_paren = char ')'

let int_literal = take_while1 (function '0' .. '9' -> true | _ -> false) >>| function s -> IntLiteral s
let identifier = take_while1 (function c -> c != ' ' && c != '\n' && c != '\r' && c != '(' && c != ')') >>| function s -> Identifier s
let spaces = skip_while (function c -> c = ' ' || c = '\n' || c = '\r')
let atom = int_literal <|> identifier >>| function a -> Atom a
let expr = fix (function expr -> 
    let tree = open_paren *> (many expr) <* close_paren >>| function s -> Tree s in
    spaces *> (atom <|> tree) <* spaces
)
let program = many expr

let parse code = match parse_string ~consume:All program code with
    | Ok v      -> v
    | Error msg -> failwith msg