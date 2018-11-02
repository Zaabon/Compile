let rec compile_com : 'a . Imp__Imp.com -> ('a Logic__Compiler_logic.hl) =
  fun cmd ->
    let res =
      begin match cmd with
      | Imp__Imp.Cskip ->
        Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.inil ())
      | Imp__Imp.Cassign (x, a) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            Compiler__Compile_aexpr.compile_aexpr a))
          (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.isetvarf x))
      | Imp__Imp.Cseq (cmd1, cmd2) ->
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            compile_com cmd1))
          (Logic__Compiler_logic.prefix_dl (compile_com cmd2))
      | Imp__Imp.Cif (cond, cmd1, cmd2) ->
        let code_false = compile_com cmd2 in
        let code_true =
          Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                              compile_com cmd1))
            (Logic__Compiler_logic.prefix_dl (Specs__VM_instr_spec.ibranchf (Z.of_int (List.length code_false)))) in
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.infix_mnmn (
                                            Logic__Compiler_logic.prefix_dl (
                                              Compiler__Compile_bexpr.compile_bexpr cond
                                                false
                                                (Z.of_int (List.length code_true))))
                                            (Logic__Compiler_logic.infix_pc code_true))
          (Logic__Compiler_logic.infix_pc (Logic__Compiler_logic.prefix_dl code_false))
      | Imp__Imp.Cwhile (test, body) ->
        let code_body = compile_com body in
        let body_length = Z.add (Z.of_int (List.length code_body)) Z.one in
        let code_test =
          Compiler__Compile_bexpr.compile_bexpr test false body_length in
        let ofs = Z.add (Z.of_int (List.length code_test)) body_length in
        let wp_while =
          Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl code_test)
            (Logic__Compiler_logic.infix_pc (Logic__Compiler_logic.infix_mnmn (
                                               Logic__Compiler_logic.prefix_dl code_body)
                                               (Logic__Compiler_logic.prefix_dl (
                                                  Specs__VM_instr_spec.ibranchf (Z.neg ofs))))) in
        Logic__Compiler_logic.infix_mnmn (Logic__Compiler_logic.prefix_dl (
                                            Specs__VM_instr_spec.inil ()))
          (Logic__Compiler_logic.make_loop wp_while)
      end in Logic__Compiler_logic.hoare res

let compile_com_natural (com: Imp__Imp.com) : Vm__Vm.instr list =
  let res = compile_com com in res

let compile_program (prog: Imp__Imp.com) : Vm__Vm.instr list =
  List.append (compile_com_natural prog) Vm__Vm.ihalt

let test (us1: unit) : Vm__Vm.instr list =
  let x = State__State.Id Z.zero in
  let y1 = State__State.Id Z.one in
  let cond =
    Imp__Imp.Bnot (Imp__Imp.Ble ((Imp__Imp.Avar y1), (Imp__Imp.Anum Z.zero))) in
  let body1 =
    Imp__Imp.Cassign (x,
      (Imp__Imp.Aadd ((Imp__Imp.Avar x), (Imp__Imp.Avar y1)))) in
  let body2 =
    Imp__Imp.Cassign (y1,
      (Imp__Imp.Asub ((Imp__Imp.Avar y1), (Imp__Imp.Anum Z.one)))) in
  let lp = Imp__Imp.Cwhile (cond, (Imp__Imp.Cseq (body1, body2))) in
  let code =
    Imp__Imp.Cseq ((Imp__Imp.Cassign (x, (Imp__Imp.Anum Z.one))), lp) in
  compile_program code

let test2 (us1: unit) : Vm__Vm.instr list =
  compile_program (Imp__Imp.Cwhile (Imp__Imp.Btrue, Imp__Imp.Cskip))

