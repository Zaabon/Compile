type pred = Vm__Vm.machine_state -> (bool)

type rel = Vm__Vm.machine_state -> (Vm__Vm.machine_state -> (bool))

type 'a pre = 'a -> ((Z.t) -> (Vm__Vm.machine_state -> (bool)))

type 'a post =
  'a -> ((Z.t) -> (Vm__Vm.machine_state -> (Vm__Vm.machine_state -> (bool))))

type 'a hl = Vm__Vm.instr list

type 'a wp_trans =
  'a -> ((Z.t) -> ((Vm__Vm.machine_state -> (bool)) -> (Vm__Vm.machine_state -> (bool))))

type 'a wp = Vm__Vm.instr list

let infix_mnmn (s1: 'a wp) (s2: ('a * Vm__Vm.machine_state) wp) : 'a wp =
  let code = List.append s1 s2 in let res = code in res

let infix_pc (s: 'a wp) : 'a wp = s

let prefix_dl (c: 'a hl) : 'a wp = c

let hoare (c: 'a wp) : 'a hl = c

let make_loop (c: 'a wp) : 'a wp = c

