type abstract_set = int

let bot = 0

let top = 1000

let join s1 s2 = 
  min top (max s1 s2)

let rec join_list l = 
  match l with 
  |[]->bot
  |x::q->join x (join_list q)

let union s1 s2 = 
  min top (max s1 s2)

