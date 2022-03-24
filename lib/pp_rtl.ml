open Rtl
open Printf

let pp_rt r :string= 
  match r with 
  |R(s)->s 
  |T(s)->s


let pp_cmp (c:cmp) = 
  match c with 
  |Eq-> "="
  |Noteq -> "!="
  |Less -> "<"
  |Lesseq -> "<="
  |Grt -> ">"
  |Grteq -> ">="


let pp_scmp sc = 
  match sc with 
  |Signed(c)-> sprintf "%s+" (pp_cmp c)
  |Unsigned(c) ->pp_cmp c

let pp_instruction (i:instructions) =
  match i with 
  |Add (a,b,c) -> sprintf "add %s, %s, %s"  (pp_rt a) (pp_rt b) (pp_rt c) 
  |Sub (a,b,c) -> sprintf "sub %s, %s, %s"  (pp_rt a) (pp_rt b) (pp_rt c) 
  |Shl (a,b,c) -> sprintf "shl %s, %s, %s"  (pp_rt a) (pp_rt b) (pp_rt c) 
  |Shr (a,b,c) -> sprintf "shr %s, %s, %s"  (pp_rt a) (pp_rt b) (pp_rt c) 
  |Asr (a,b,c) -> sprintf "asr %s, %s, %s"  (pp_rt a) (pp_rt b) (pp_rt c) 
  |Set (a,b) -> sprintf "set %s, %s,"  (pp_rt a) (pp_rt b)
  |Seti (a,i) -> sprintf "seti %s, %i" (pp_rt a) i
  |Scratch a-> sprintf "scratch %s" (pp_rt a)
  |Store (a,b,_) -> sprintf "store %s, %s, %s" (pp_rt a) (pp_rt b) "generic_type" 
  |Load (a,b,_) -> sprintf "load %s, %s, %s" (pp_rt a) (pp_rt b) "generic_type" 
  |Cmp (a,b,c) -> sprintf "cmp %s, %s, %s" (pp_rt a) (pp_rt b) (pp_rt c)
  |If (sc,a,i) -> sprintf "if %s, %s, %i" (pp_scmp sc) (pp_rt a) i
  |Branch(l) -> sprintf "branch %s" l
  |Cont -> sprintf "cont"

let pp_block b = 
  List.fold_left (
    fun s i -> sprintf "%s\n \t %s" s (pp_instruction i)
  ) "" b


let pp_program p = 
  Hashtbl.fold (fun l b s -> sprintf "%s\n.%s\n%s" s l (pp_block b) ) p.blocks ""

