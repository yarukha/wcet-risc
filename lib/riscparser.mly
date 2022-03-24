%{

  open Lexing
  open Risc

%}

%token COMA 
%token BEGIN
%token L_BRACE
%token R_BRACE
%token DOT

%token <string> STRING 
%token <int> INT

%token J JR 
%token MV
%token ADDI SW LW SLLI ADD BLT 

%token EOF

%start program
%type  <Risc.program> program 
%%

program: 
    |p= list(line) ;EOF {p}
    |EOF    {failwith "empty file"}
    ;

line:
    |i = ident ; BEGIN       {Ident (i)} 
    |op = op1; v1 = value   {Instr (Op (op, v1))}
    |op = op2 ; v1 = value; COMA; v2=value  {Instr(Op2 (op,v1,v2))}
    |op = op3; v1 = value; COMA; v2=value; COMA; v3 = value   {Instr(Op3 (op,v1,v2,v3))}
    |op = op3; v1 = value; COMA; v2=value; L_BRACE; v3 = value; R_BRACE    {Instr(Op3 (op,v1,v2,v3))}
    ;

ident: 
    |DOT; l = STRING {Id (l)} 
    |f=STRING {Function (f)} 

op1:
    |J {J}
    |JR {Jr}
    ;

op2:
    |MV {Mv}
    ;

op3:
    |ADDI {Addi}
    |SW {Sw}
    |LW {Lw}
    |SLLI {Slli}
    |ADD {Add}
    |BLT {Blt}
    ;

value:
    |r = STRING {R (r)}
    |i = INT    {Int (i)}
    |DOT; l= STRING { L (l) };
