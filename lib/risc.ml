type unop = J | Jr
type binop = Mv
type triop = Addi | Sw | Lw | Slli | Add | Blt 

type label = string

type value = R of string | Int of int | L of label

type instruction = 
      |Op of unop * value
      |Op2 of binop * value * value  
      |Op3 of triop * value * value * value


type line = Instr of instruction | Label of label

type  program = line list 