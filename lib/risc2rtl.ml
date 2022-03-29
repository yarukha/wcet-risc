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


let translate_program (p:Risc.program) = 
  let blocks = Hashtbl.create 32 in 
  let add_block b l = 
    print_string ("added the label "^l^"\n");
    (*we reverse the block here*)
    Hashtbl.add blocks l (List.rev b) 
  in

  let translate_instr (i:Risc.instruction) current_block  :block= 

    match i with 
    |Op (op,v) -> (
      let l = match v with 
      |R(s)->s
      |_->failwith "int here" 
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
    )
    |Op3 (op,v1,v2,v3)->(
      let r1 = value_to_reg v1 
      and r2 = value_to_reg v2 and i2 = value_to_int v2 
      and r3 = value_to_reg v3 and i3 = value_to_int v3
      in
      match op with 
      |Addi -> 
        let b = Seti(t0,i3)::current_block in 
        Add(r1,r2,t0)::b
      |Sw -> 
        let b = Seti(t0,i2)::current_block in 
        let b = Add(t0,r3, t0)::b in 
        Store(r1, t0, A)::b
      |Lw -> 
        let b = Seti(t0,i2)::current_block in 
        let b = Add(t0,r3,t0)::b in 
        Load(r1,t0,A)::b
      |Slli -> 
        let b = Seti(t0,i3)::current_block in 
        Shl(r1,r2,t0)::b
      |Add -> 
        Add(r1,r2,r3)::current_block
      |Blt -> 
        let b = Cmp(t0,r1,r2)::current_block in 
        If(Signed(Less),t0,i3)::b
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




      
