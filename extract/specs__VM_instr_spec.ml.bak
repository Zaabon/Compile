let ifunf (code_f: Vm__Vm.instr list) : 'a Logic__Compiler_logic.hl = code_f

let iconstf (n: Z.t) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.iconst n)))

let ivarf (x: State__State.id) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf (
                                                                  Vm__Vm.ivar x)))

type binop = (Z.t) -> ((Z.t) -> (Z.t))

let create_binop (code_b: Vm__Vm.instr list) : 'a Logic__Compiler_logic.hl =
  Logic__Compiler_logic.hoare (Logic__Compiler_logic.prefix_dl (ifunf code_b))

let iaddf (us1: unit) : 'a Logic__Compiler_logic.hl =
  create_binop Vm__Vm.iadd

let iadduf (us1: unit) : 'a Logic__Compiler_logic.hl =
  create_binop Vm__Vm.iaddu

let isubf (us1: unit) : 'a Logic__Compiler_logic.hl =
  create_binop Vm__Vm.isub

let inil (us1: unit) : 'a Logic__Compiler_logic.hl = []

let ibranchf (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  let cf = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.ibranch ofs)) in
  Logic__Compiler_logic.hoare cf

type cond = (Z.t) -> ((Z.t) -> (bool))

let create_cjump (code_cd: Vm__Vm.instr list) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf code_cd) in
  Logic__Compiler_logic.hoare c

let ibeqf (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ibeq ofs)

let ibnef (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ibne ofs)

let iblef (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ible ofs)

let ibgtf (ofs: Z.t) : 'a Logic__Compiler_logic.hl =
  create_cjump (Vm__Vm.ibgt ofs)

let isetvarf (x: State__State.id) : 'a Logic__Compiler_logic.hl =
  let c = Logic__Compiler_logic.prefix_dl (ifunf (Vm__Vm.isetvar x)) in
  Logic__Compiler_logic.hoare c

