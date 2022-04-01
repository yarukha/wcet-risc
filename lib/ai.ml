open Rtl 
open Abstract_set


let update_instr (inst:instructions) s = 
  match inst with
  |_-> union 0 (s + 1)

let rec update_block b s = 
  let rec nth_list l n = 
    match l with 
    |[]->[]
    |x::q-> if n =0 then x::q else nth_list q (n-1)
  in
  match b with 
  |[]->s 
  |inst::q->(
    let s' = update_instr inst s in 
    match inst with 
    |If(_,_,offset)->
      union (update_block q s') (update_block (nth_list b offset) s')
    |_-> update_block q s' 
  )

        

let interpretation prog = 
  (*All of this is just to initialise hashtbl of predecessors and successors of each labels*)
  let blocks = prog.blocks in 
  let successors = Hashtbl.create 32 in 
  let predecessors = Hashtbl.create 32 in 
  (*we create both tables at the same time*)
  Hashtbl.iter (
    fun l _ -> Hashtbl.add successors l []; Hashtbl.add predecessors l []) blocks;
  let get_successors b = 
    let rec foo b' stack = match b' with |[]->stack |Branch(l)::q-> foo q (l::stack) |_::q -> foo q stack 
  in foo b []

  in let update_predecessors l succ_list = 
    List.iter (
      fun s -> let pred_list = Hashtbl.find predecessors s in 
              Hashtbl.replace predecessors s (l::pred_list)) succ_list
  in
  (*update the successors*)
  Hashtbl.iter (
    fun l b -> Hashtbl.replace successors l (get_successors b)
  ) blocks;
  (*now update the predecessors*)
  Hashtbl.iter update_predecessors successors;

  (*Now for the real interpretation*)
  let abstract_values = Hashtbl.create 32 in 
  (*all blocks are first interpreted as bot*)
  Hashtbl.iter (fun l _ -> Hashtbl.add abstract_values l bot) blocks;


  let rec run_through_cfg l = 
    let succ_list = Hashtbl.find successors l in 
    (* Printf.printf "%s successors:" l;
    List.iter (fun s -> Printf.printf " %s " s) succ_list;
    print_newline (); *)

    let pred_list = Hashtbl.find predecessors l in 
    (* Printf.printf "%s predecessors:" l;
    List.iter (fun s -> Printf.printf " %s " s) pred_list;
    print_newline (); *)

    let values_of_pred = List.map (fun p -> Hashtbl.find abstract_values p) pred_list in 
    let b = Hashtbl.find blocks l in 
    let old_value = Hashtbl.find abstract_values l in 
    (* Printf.printf "join value of pred %i\n" (join_list values_of_pred); *)
    let new_value = update_block b (join_list values_of_pred) in 
    (* Printf.printf "%s old %i new %i\n\n" l old_value new_value; *)
    if old_value != new_value then (
      Hashtbl.replace abstract_values l new_value;
      List.iter  run_through_cfg succ_list)
    else ();    
    
  in run_through_cfg prog.entry;
  abstract_values 





let calcul_wcet prog = 
  let abstract_values = interpretation prog in 
  Hashtbl.fold (fun _ v m -> max v m) abstract_values 0
