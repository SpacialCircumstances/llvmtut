type value = 
    | Literal of int64
    | Variable of string

type ir_expr =
    | Add of value * value
    | Sub of value * value
    | Div of value * value
    | Mul of value * value

type statement =
    | Print of value
    | Set of string * value
    | If of value * block * block

and block = statement list * value

type top_level =
    | DefFunction of (string list) * block
    | BasicStatement of statement

type ir_module = top_level list * value