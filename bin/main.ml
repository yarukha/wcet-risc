open Risc
open Rtl

let () =
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in 
  let prog =Riscparser.program Risclexer.token lb in 
  close_in c;;
