(**
  Intermediate language using simple register transfer, organised in blocks of instructions*)

type registers = string
type temporaries = string
type label = string
type rt = R of registers | T of temporaries 
type value = int

type int_type = A
type cmp = Eq | Noteq | Less | Lesseq | Grt | Grteq 
type scmp = Signed of cmp | Unsigned of cmp 

type instructions = 
  |Add of rt * rt *rt
  |Sub of rt * rt *rt
  |Shl of rt * rt *rt
  |Shr of rt * rt *rt
  |Asr of rt * rt *rt
  |Set of rt * rt
  |Seti of rt * value
  |Scratch of rt 
  |Store of rt * rt * int_type
  |Load of rt * rt * int_type
  |Cmp of rt * rt * rt 
  |If of scmp  * rt * value 
  |Branch of label
  |Cont




type block = instructions list
type program = {
  entry: label; 
  blocks: (label, block) Hashtbl.t
}




