type monop = Nop
type unop = J | Jr |Call
type binop = Mv |Li
type triop = 
      Addi | Sw | Lw | Slli | Add | Blt 
      |Fld | Fsd | Fgt_d | Beq
      |Bne |Mul

type label = string

type value = R of string | Int of int

type instruction = 
      |Monop of monop
      |Op of unop * value
      |Op2 of binop * value * value  
      |Op3 of triop * value * value * value


type line = Instr of instruction | Label of label

type  program = line list 