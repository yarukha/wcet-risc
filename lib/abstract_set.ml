type abstract_set = int

let bot = 0

let top = 100


let union s1 s2 = 
  min top (max s1 s2)


let join s1 s2 = 
  union s1 s2

let rec join_list l = 
  match l with 
  |[]->bot
  |x::q->let y = join_list q in join x y