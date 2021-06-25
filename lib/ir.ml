type value = 
    | Literal of int64
    | Variable of string

type ir =
    | Add of value * value
    | Sub of value * value
    | Div of value * value
    | Mul of value * value

type statement =
    | Print of ir
    | Set of string * ir

type top_level =
    | Define of (string list) * (statement list)

type ir_module = {
    top_levels: top_level list;
    statements: statement list
}