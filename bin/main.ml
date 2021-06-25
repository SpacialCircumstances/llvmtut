let () =
    let filename = Sys.argv.(1) in
    Llvmtut.Compiler.compile filename