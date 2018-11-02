exception Err

exception Halt of Vm__Vm.machine_state

let rec split_at (p: Z.t) (c: Vm__Vm.instr list) :
  (Vm__Vm.instr list) * (Vm__Vm.instr list) =
  begin match (c, Z.equal p Z.zero) with
  | ([], _) -> raise Err
  | (_, true) -> ([], c)
  | (e :: cqt, _) ->
    let (hd, tl) = split_at (Z.sub p Z.one) cqt in (e :: hd, tl)
  end

let pop (s: (Z.t) list) : (Z.t) * ((Z.t) list) =
  begin match s with
  | [] -> raise Err
  | rv :: rs -> (rv, rs)
  end

let instr_ex (c: Vm__Vm.instr list) (ms: Vm__Vm.machine_state) : Vm__Vm.machine_state
  =
  let Vm__Vm.VMS (p, s, m) = ms in
  begin
    if Z.lt p Z.zero then begin raise Err end;
    begin try
      begin match split_at p c with
      | (_, instr :: _) ->
        begin match instr with
        | Vm__Vm.Iconst n ->
          Vm__Vm.VMS ((Z.add p Z.one), (Vm__Vm.push n s), m)
        | Vm__Vm.Ivar id ->
          Vm__Vm.VMS ((Z.add p Z.one),
            (Vm__Vm.push (State__State.mixfix_lbrb m id) s), m)
        | Vm__Vm.Isetvar id ->
          let (rv, rs) = pop s in
          Vm__Vm.VMS ((Z.add p Z.one), rs,
            (State__State.mixfix_lblsmnrb m id rv))
        | Vm__Vm.Ibranch ofs ->
          Vm__Vm.VMS ((Z.add (Z.add p Z.one) ofs), s, m)
        | Vm__Vm.Iadd ->
          let (rv1, rs1) = pop s in let (rv2, rs2) = pop rs1 in
          Vm__Vm.VMS ((Z.add p Z.one), (Vm__Vm.push (Z.add rv1 rv2) rs2), m)
        | Vm__Vm.Iaddu ->
          let (rv11, rs3) = pop s in let (rv21, rs4) = pop rs3 in
          Vm__Vm.VMS ((Z.add p Z.one),
            (Vm__Vm.push (Bv_op__BV_OP.bv_add rv11 rv21) rs4), m)
        | Vm__Vm.Isub ->
          let (rv12, rs5) = pop s in let (rv22, rs6) = pop rs5 in
          Vm__Vm.VMS ((Z.add p Z.one), (Vm__Vm.push (Z.sub rv22 rv12) rs6),
            m)
        | Vm__Vm.Ibeq ofs ->
          let (rv13, rs7) = pop s in let (rv23, rs8) = pop rs7 in
          begin match Z.equal rv13 rv23 with
          | true -> Vm__Vm.VMS ((Z.add (Z.add p Z.one) ofs), rs8, m)
          | false -> Vm__Vm.VMS ((Z.add p Z.one), rs8, m)
          end
        | Vm__Vm.Ibne ofs ->
          let (rv14, rs9) = pop s in let (rv24, rs10) = pop rs9 in
          begin match not (Z.equal rv14 rv24) with
          | true -> Vm__Vm.VMS ((Z.add (Z.add p Z.one) ofs), rs10, m)
          | false -> Vm__Vm.VMS ((Z.add p Z.one), rs10, m)
          end
        | Vm__Vm.Ible ofs ->
          let (rv15, rs11) = pop s in let (rv25, rs12) = pop rs11 in
          begin match Z.leq rv25 rv15 with
          | true -> Vm__Vm.VMS ((Z.add (Z.add p Z.one) ofs), rs12, m)
          | false -> Vm__Vm.VMS ((Z.add p Z.one), rs12, m)
          end
        | Vm__Vm.Ibgt ofs ->
          let (rv16, rs13) = pop s in let (rv26, rs14) = pop rs13 in
          begin match Z.gt rv26 rv16 with
          | true -> Vm__Vm.VMS ((Z.add (Z.add p Z.one) ofs), rs14, m)
          | false -> Vm__Vm.VMS ((Z.add p Z.one), rs14, m)
          end
        | Vm__Vm.Ihalt -> raise (Halt ms)
        end
      | _ -> assert false (* absurd *)
      end with
    | Err -> raise Err
    end
  end

let rec instr_iter_ex (c: Vm__Vm.instr list) (ms: Vm__Vm.machine_state) :
  Vm__Vm.machine_state =
  begin try let o = instr_ex c ms in instr_iter_ex c o with
  | Err -> raise Err
  | Halt ms1 -> raise (Halt ms1)
  end

