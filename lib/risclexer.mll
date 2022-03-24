{
    open Lexing
    open Riscparser

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
            |s -> STRING(s)
}

let digit = ['0'-'9']
let number = ['-']? digit+
let alpha = ['a'-'z' 'A'-'Z']
let ident = ['a'-'z' '_'] (alpha | '_' | digit)*


rule token = parse
    | ['\n']    { new_line lexbuf; token lexbuf }
    | [' ' '\t' '\r']+  { token lexbuf }
    | "//" [^ '\n']* "\n"   { new_line lexbuf; token lexbuf }
    | "," {COMA}
    | ":" {BEGIN}
    | "(" {L_BRACE}
    | ")" {R_BRACE}
    | "." {DOT}
    |ident as s {try_string s}
    |number as i    {INT( int_of_string i)}
    |_ { failwith ("Unknown character : " ^ (lexeme lexbuf)) }
    |eof {EOF }