open Rtl
let t0 = Rtl.T("t0")
let t1 = Rtl.T("t1")



let value_to_int v :Rtl.value= 
  match v with 
  |Risc.Int(i)->i
  |_->0

let value_to_reg v = 
  match v with 
  |Risc.R(r)->R(r)
  |Risc.Int(i)-> R(Printf.sprintf "int %i" i)

let value_to_label (rt:Risc.value) = 
  match rt with 
  |R(l)->l
  |_->failwith "conversion of register to label failed"

let translate_program (p:Risc.program) = 
  let blocks = Hashtbl.create 32 in 
  let add_block b l = 
    print_string ("added the label "^l^"\n");
    (*we reverse the block here*)
    Hashtbl.add blocks l (List.rev b) 
  in

  let translate_instr (i:Risc.instruction) current_block  :block= 

    match i with 
    |Monop(_)->Scratch (R("x0"))::current_block
    |Op (op,v) -> (
      let l = value_to_label v
    in 
    match op with
      |J -> Branch(l)::current_block
      |Jr-> Scratch(value_to_reg v)::current_block
      )

    |Op2 (op,v1,v2) -> (
      let r1 = value_to_reg v1 and r2 = value_to_reg v2 in 
      match op with 
      |Mv -> 
        Set(r1,r2)::current_block

        (*this needs change obviously*)
      |_-> Scratch(R("x0"))::current_block
    )
    |Op3 (op,v1,v2,v3)->(
      let r1 = value_to_reg v1 
      and r2 = value_to_reg v2 and i2 = value_to_int v2 
      and r3 = value_to_reg v3 and i3 = value_to_int v3
      in
      match op with 
      |Addi -> 
        Add(r1,r2,t0)::Seti(t0,i3)::current_block
      |Sw -> 
        Store(r1, t0, A)::Add(t0,r3, t0)::Seti(t0,i2)::current_block
      |Lw -> 
        Load(r1,t0,A)::Add(t0,r3,t0)::Seti(t0,i2)::current_block
      |Slli -> 
        Shl(r1,r2,t0)::Seti(t0,i3)::current_block
      |Add -> 
        Add(r1,r2,r3)::current_block
      |Blt -> 
        Branch(value_to_label v3)::If(Signed(Less),t0,i3)::Cmp(t0,r1,r2)::current_block 
      |Fld -> 
        Load(r1,t0,B)::Add(t0,r3,t0)::Seti(t0,i2)::current_block
      |Fsd ->
        Store(r1, t0, B)::Add(t0,r3, t0)::Seti(t0,i2)::current_block
      |Beq ->
        Branch(value_to_label v3)::If(Unsigned(Eq),t0,1)::Cmp(t0,r1,r2)::current_block

        
        (*obviously this need to be changed*)
      |_-> Scratch(R("x0"))::current_block
    )
    
  in 
  let rec translate_line_list (p':Risc.program) current_block current_label= 
    match p' with 
    |[]->add_block current_block current_label
    |line::q -> 
      match line with 
      |Instr(i)->
        translate_line_list q (translate_instr i current_block) current_label
      |Label(l)->
        add_block current_block current_label;
        translate_line_list q [] l 

  in 
  match p with 
  |[]->failwith "empty risc program"
  |Label(l)::q-> 
    translate_line_list q [] l; 
    {entry = l; blocks = blocks}
  |_->failwith "no label at beginning of risc program"




      
