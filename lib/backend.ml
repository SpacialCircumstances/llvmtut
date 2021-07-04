open Llvm
open Llvm_target
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

let compile_value ctx v = match v with
    | ErrValue -> failwith "Unexpected ErrValue"
    | Literal i -> const_int number_type (Int64.to_int i)
    | Variable name -> ValueTable.find ctx.values name

let compile_expr ctx expr = match expr with
    | Value v -> compile_value ctx v
    | _ -> failwith "Not implemented"

let compile_statement ctx st = match st with
    | Set (name, expr) -> 
        let value = compile_expr ctx expr in
        ValueTable.add ctx.values name value
    | _ -> failwith "Not implemented"

let compile_top_level _ctx tl = match tl with
    | DefFunction (_params, _block) -> () 

let compile_main_function ctx irmod =
    let mf = declare_function "main" (function_type number_type Array.empty) mdl in
    let block = append_block context "entry" mf in
    position_at_end block builder;
    List.iter (compile_statement ctx) irmod.statements;
    let ret = compile_value ctx irmod.retval in
    let _ = build_ret ret builder in
    Llvm_analysis.assert_valid_function mf;
    mf

let generate_native_code irmod = 
    let ctx = { values = ValueTable.create 20 } in
    let _ = List.iter (compile_top_level ctx) irmod.top_levels in
    let _mf = compile_main_function ctx irmod in
    let _ = dump_module mdl in
    let tt = Target.default_triple () in
    Llvm_all_backends.initialize ();
    print_endline ("Target triple: " ^ tt);
    let target = Target.by_triple tt in
    let tm = TargetMachine.create target ~triple:tt in
    let data_layout = TargetMachine.data_layout tm in
    set_data_layout (DataLayout.as_string data_layout) mdl;
    set_target_triple tt mdl;
    TargetMachine.emit_to_file mdl CodeGenFileType.ObjectFile "test.obj" tm;
    ()