open Rtl 

let nb_boucle = 10
let mesure_program prog= 
  let blocks = prog.blocks in 
  let rec mesure_block b = 
    match b with 
    |[]->0
    |Branch(l)::q->
      let new_block = Hashtbl.find blocks l in 
      nb_boucle * (mesure_block new_block) + mesure_block q 
    |_::q-> 1 + mesure_block q
  in 
  let entry_block = Hashtbl.find blocks (prog.entry) in 
  mesure_block entry_block

  
  
