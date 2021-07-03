open Llvm
open Ir
open Containers

let context = global_context ()
let mdl = create_module context "tut"
let builder = builder context
let number_type = i64_type context

module ValueTable = CCHashtbl.Make(String)

type ctx = {
    values: llvalue ValueTable.t
}

let compile_statement _ctx _st = ()

let compile_top_level _ctx tl = match tl with
    | DefFunction (_params, _block) -> () 

let generate_native_code irmod = 
    let ctx = { values = ValueTable.create 20 } in
    List.iter (compile_top_level ctx) irmod.top_levels;
    List.iter (compile_statement ctx) irmod.statements;
    ()