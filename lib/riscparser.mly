%{

  open Risc

%}

%token COMA 
%token BEGIN
%token L_BRACE
%token R_BRACE

%token <string> STRING 
%token <int> INT

%token NOP RET
%token J JR CALL
%token MV LI
%token ADDI SW LW SLLI ADD BLT FLD FSD FGT_D BEQ BNE MUL LUI

%token EOF

%start program
%type  <Risc.program option> program 
%%

program: 
    |p= nonempty_list(line) EOF {Some(p)}
    |EOF    {None}
    ;

line:
    |l = STRING ; BEGIN       {Label (l)} 
    |RET {Inst(Ret,Null,Null,Null)}
    |NOP {Inst(Nop,Null,Null,Null)}
    |op = op; v1 = value   {Inst (op, v1,Null,Null)}
    |op = op ; v1 = value; COMA; v2=value  {Inst(op,v1,v2,Null)}
    |op = op; v1 = value; COMA; v2=value; COMA; v3 = value   {Inst(op,v1,v2,v3)}
    |op = op; v1 = value; COMA; v2=value; L_BRACE; v3 = value; R_BRACE  {Inst(op,v1,v2,v3)}
    ;

op:
    |J {J}
    |JR {Jr}
    |CALL {Call}
    |MV {Mv}
    |LI {Li}
    |ADDI {Addi}
    |SW {Sw}
    |LW {Lw}
    |SLLI {Slli}
    |ADD {Add}
    |BLT {Blt}
    |FLD {Fld}
    |FSD {Fsd}
    |FGT_D {Fgt_d}
    |BEQ {Beq}
    |BNE {Bne}
    |MUL {Mul}
    |LUI {Lui}
    ;

value:
    |r = STRING { R (r)}
    |i = INT    {Int (i)}

