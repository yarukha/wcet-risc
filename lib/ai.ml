open Rtl 
open Abstract_set


let update_instr (inst:instructions) s = 
  match inst with
  |_-> union s 1

let update_block b s = 
  let n = List.length b in 
  (*subfunction maintaining the working list of (int,sets)*)
  let rec foo working_list return_state = 
    match working_list with 
    |[]->return_state 
    |(i,s')::q-> (
      if i >= n then 
        union s' return_state 
      else 
      match List.nth b i with 
      |If(_,_,d)->foo ((i+1,s')::(i+d,s')::q) return_state
      |instr->foo ((i+1,update_instr instr s')::q) return_state
    )
  in foo [(0,s)] bottom 
        
