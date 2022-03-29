open Wcetrisc


let () =
  let file = Sys.argv.(1) in 
  let c = open_in file in 
  let lb = Lexing.from_channel c in
  let prog = Loadrisc.prog lb in 
  close_in c;
  let rtl_p = Risc2rtl.translate_program prog in 
  let output_file = (Filename.chop_suffix file ".risc")^".rtl" in
  let out = open_out output_file in
  Printf.fprintf out "%s" (Pp_rtl.pp_program rtl_p);
  print_string (Printf.sprintf "%i\n" (Ai.mesure_program rtl_p));;

