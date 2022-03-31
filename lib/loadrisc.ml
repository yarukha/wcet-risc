open Lexing
let print_position outx lb =
  let pos = lb.lex_curr_p in
  Printf.fprintf outx "%s:%d:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_errors lexbuf =
  try Riscparser.program Risclexer.token lexbuf with 
  |Risclexer.SyntaxError msg ->
    Printf.fprintf stderr "%a: %s\n" print_position lexbuf msg;
    None
  |Riscparser.Error ->
    Printf.fprintf stderr "%a: syntax error\n" print_position lexbuf;
    exit (-1)
  

let prog lb = 
  match parse_with_errors lb with 
  |Some(p)->p 
  |None-> 
    failwith "empty file"