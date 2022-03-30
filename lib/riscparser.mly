%{

  open Risc

%}

%token COMA 
%token BEGIN
%token L_BRACE
%token R_BRACE

%token <string> STRING 
%token <int> INT

%token NOP
%token J JR CALL
%token MV LI
%token ADDI SW LW SLLI ADD BLT FLD FSD FGT_D BEQ BNE MUL

%token EOF

%start program
%type  <Risc.program> program 
%%

program: 
    |p= list(line) ;EOF {p}
    |EOF    {failwith "empty file"}
    ;

line:
    |i = ident ; BEGIN       {Label (i)} 
    |NOP {Instr(Monop(Nop))}
    |op = op1; v1 = value   {Instr (Op (op, v1))}
    |op = op2 ; v1 = value; COMA; v2=value  {Instr(Op2 (op,v1,v2))}
    |op = op3; v1 = value; COMA; v2=value; COMA; v3 = value   {Instr(Op3 (op,v1,v2,v3))}
    |op = op3; v1 = value; COMA; v2=value; L_BRACE; v3 = value; R_BRACE    {Instr(Op3 (op,v1,v2,v3))}
    ;

ident:
    |f=STRING {f} 


op1:
    |J {J}
    |JR {Jr}
    |CALL {Call}
    ;

op2:
    |MV {Mv}
    |LI {Li}
    ;

op3:
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
    ;

value:
    |r = STRING { R (r)}
    |i = INT    {Int (i)}

