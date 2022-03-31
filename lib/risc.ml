type op = 
      Nop |Ret
      |J |Jr |Call
      |Mv |Li
      |Addi | Sw | Lw | Slli | Add | Blt 
      |Fld | Fsd | Fgt_d | Beq
      |Bne |Mul |Lui

type label = string

type value = R of string | Int of int |Null

type instruction = op * value * value * value


type line = Inst of instruction | Label of label

type  program = line list 