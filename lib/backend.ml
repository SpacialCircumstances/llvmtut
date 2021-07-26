open Llvm
open Llvm_target
open Ir
open Containers

let context = global_context ()
let mdl = create_module context "tut"
let builder = builder context
let number_type = i64_type context
let void_type = void_type context
let zero = const_int number_type 0

module ValueTable = CCHashtbl.Make(String)

type ctx = {
    values: llvalue ValueTable.t;
    builtins: llvalue ValueTable.t;
}

let compile_value ctx v = match v with
    | ErrValue -> failwith "Unexpected ErrValue"
    | Literal i -> const_int number_type (Int64.to_int i)
    | Variable name -> ValueTable.find ctx.values name

let compile_expr ctx expr = match expr with
    | Value v -> compile_value ctx v
    | Add (v1, v2) -> 
        let v1 = compile_value ctx v1 in
        let v2 = compile_value ctx v2 in
        build_add v1 v2 "addtmp" builder
    | Sub (v1, v2) -> 
        let v1 = compile_value ctx v1 in
        let v2 = compile_value ctx v2 in
        build_sub v1 v2 "subtmp" builder
    | Mul (v1, v2) ->
        let v1 = compile_value ctx v1 in
        let v2 = compile_value ctx v2 in
        build_mul v1 v2 "multmp" builder
    | Div (v1, v2) -> 
        let v1 = compile_value ctx v1 in
        let v2 = compile_value ctx v2 in
        build_udiv v1 v2 "divtmp" builder
    | FunctionCall (name, args) ->
        let func = match lookup_function name mdl with
                    | Some f -> f
                    | None -> failwith ("Function " ^ name ^ " not found in LLVM module")
        in
        let args = List.map (compile_value ctx) args |> Array.of_list in
        build_call func args "calltmp" builder
    | Read ->
        let rf = ValueTable.find ctx.builtins "sl_read" in
        build_call rf [||] "readtmp" builder

let compile_block ctx (statements, ret) compile_statement = 
    List.iter (compile_statement ctx) statements;
    let rv = compile_value ctx ret in
    build_ret rv builder

let rec compile_statement ctx st = match st with
    | Set (name, expr) -> 
        let value = compile_expr ctx expr in
        ValueTable.add ctx.values name value
    | Print value ->
        let value = compile_value ctx value in
        build_call (ValueTable.find ctx.builtins "sl_print") [|value|] "" builder |> ignore
    | If (cond, if_true, if_false) ->
        let cond_v = compile_value ctx cond in
        let cond_cmp = build_icmp Icmp.Eq cond_v zero "condtmp" builder in
        let main_block = insertion_block builder in
        let func = block_parent main_block in
        let true_block = append_block context "iftrue" func in
        let false_block = append_block context "iffalse" func in
        position_at_end true_block builder;
        let true_val = compile_block ctx if_true compile_statement in
        let true_block_final = insertion_block builder in
        position_at_end false_block builder;
        let false_val = compile_block ctx if_false compile_statement in
        let false_block_final = insertion_block builder in
        let final_block = append_block context "ifafter" func in
        position_at_end final_block builder;
        let phi = build_phi [ (true_val, true_block_final); (false_val, false_block_final) ] "ifphi" builder in
        position_at_end main_block builder;
        build_cond_br cond_cmp true_block false_block builder |> ignore;
        position_at_end true_block_final builder;
        build_br final_block builder |> ignore;
        position_at_end false_block_final builder;
        build_br final_block builder |> ignore;
        position_at_end final_block builder;
        phi |> ignore

let compile_function ctx fname args statements retval =
    let f = declare_function fname (function_type number_type (Array.make (List.length args) number_type)) mdl in
    Array.iteri (fun idx a -> let av = List.nth args idx in
                                set_value_name av a;
                                ValueTable.add ctx.values av a) (params f);
    let block = append_block context "entry" f in
    position_at_end block builder;
    compile_block ctx (statements, retval) compile_statement |> ignore;
    Llvm_analysis.assert_valid_function f

let compile_top_level ctx tl = match tl with
    | DefFunction (fname, params, (statements, retval)) -> 
        compile_function ctx fname params statements retval

let generate_native_code irmod = 
    let sl_print = declare_function "sl_print" (function_type void_type [|number_type|]) mdl in
    let sl_read = declare_function "sl_read" (function_type number_type  [||]) mdl in
    let builtins = [
        ("sl_print", sl_print);
        ("sl_read", sl_read)
    ] in
    let ctx = { values = ValueTable.create 20; builtins = ValueTable.of_list builtins } in
    List.iter (compile_top_level ctx) irmod.top_levels;
    compile_function ctx "main" [] irmod.statements irmod.retval;
    dump_module mdl;
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