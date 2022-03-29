type abstract_set = int 


let bottom = 0

let join s1 s2 = 
  Int.max s1 s2

let rec join_list l = 
  match l with 
  |[]->bottom
  |x::q->join x (join_list q)

let union s1 s2 :abstract_set = 
  Int.add s1 s2 

