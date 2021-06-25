open Core

let compile filename = 
    let text = In_channel.read_all filename in
    let _ast = Parser.parse text in
    ()