open Rtl 
open Abstract_set


let update_instr (inst:instructions) s = 
  match inst with
  |_-> s + 1

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
  in foo [(0,s)] bot
        

let interpretation prog = 

  Printf.printf "the stored labels are %s\n\n" (Hashtbl.fold (fun l _ s -> s^" "^l) prog.blocks "");


  (*NEEDS TO BE DEBUGGED*)
  (*all of this is to create a nice connection between each block and its predecessors*)
  let pred_labels = Hashtbl.create 32 in 
  (*this ensures that all the predecessors lists are initialised*)
  Hashtbl.iter (fun l _ -> Hashtbl.add pred_labels l []) prog.blocks;
  let update_pred l pred = 
    let current_preds = Hashtbl.find pred_labels l in
    Hashtbl.add pred_labels l (pred::current_preds) 
  in
  let rec update_pred_from_block l block = 
    match block with 
    |[]->()
    |Branch(pred)::q->
      update_pred l pred;
      update_pred_from_block l q
    |_::q->update_pred_from_block l q
  in
  Hashtbl.iter update_pred_from_block prog.blocks;

  (*used for debug*)
  Hashtbl.iter (
    fun l pred -> print_string (
      (List.fold_left (fun a b -> a^" "^b)  (l^" preds= ") pred)^"\n";
      )
      ) pred_labels;
  print_newline ();

  let abstract_values = Hashtbl.create 32 in 
  Hashtbl.iter (fun l _ -> Hashtbl.add abstract_values l bot) prog.blocks;
  let rec foo working_list = 
    match working_list with 
    |[]->();
    |l::q->
      let pred_list = Hashtbl.find pred_labels l in 
      let pred_abstraction_list = List.map (fun l' -> Hashtbl.find abstract_values l') pred_list in
      let s = update_block (Hashtbl.find prog.blocks l) (join_list pred_abstraction_list ) in 
      if s != (Hashtbl.find abstract_values l) then (
        Hashtbl.add abstract_values l s;
        foo (pred_list@q))
      else foo q
  in foo [prog.entry];

  abstract_values




let calcul_wcet prog = 
  let abstract_values = interpretation prog in 
  print_int (Hashtbl.length abstract_values);
  Hashtbl.iter (fun l s -> print_string (Printf.sprintf "%s wcet= %i\n" l s)) abstract_values;
  Hashtbl.fold (fun _ a b -> max a b) abstract_values 0