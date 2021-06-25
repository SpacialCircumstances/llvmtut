type atom =
    | Identifier of string
    | IntLiteral of string

type expr = 
    | Atom of atom
    | Tree of expr list

let rec to_string expr = match expr with
    | Atom (Identifier id) -> id
    | Atom (IntLiteral il) -> il
    | Tree exprs -> "(" ^ (String.concat " " (List.map to_string exprs)) ^ ")"