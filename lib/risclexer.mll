{
    open Lexing
    open Riscparser

    exception SyntaxError of string

        let try_string  s= 
            match s with 
            |"j" -> J 
            |"jr" -> JR
            |"mv" -> MV
            |"addi" -> ADDI 
            |"sw" -> SW
            |"lw" -> LW
            |"slli" -> SLLI
            |"add" -> ADD
            |"blt" -> BLT 
            |"fsd"->FSD
            |"fld"-> FLD
            |"fgt.d"->FGT_D
            |"beq"->BEQ
            |"li"->LI
            |"nop"->NOP
            |"bne"->BNE 
            |"call"->CALL 
            |"mul"->MUL
            |"lui"->LUI
            |"ret"->RET
            

            |s -> STRING(s)
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_' 'A'-'Z' '.'] (alpha | '_' | '.' | digit)*

rule token = parse
    | ['\n']    { new_line lexbuf; token lexbuf}
    | [' ' '\t' '\r']+  { token lexbuf }
    | "//" [^ '\n']* "\n"   { new_line lexbuf; token lexbuf}
    | "," {COMA}
    | ":" {BEGIN}
    | "(" {L_BRACE}
    | ")" {R_BRACE}
    | "#" {comment lexbuf}
    |ident as s {try_string s}
    |number as i    {INT( int_of_string i)}
    |_ { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
    |eof {EOF }

and comment = parse 
    |"\n" {new_line lexbuf; token lexbuf}
    |_ {comment lexbuf}
    |eof {failwith "unfinished comment" }