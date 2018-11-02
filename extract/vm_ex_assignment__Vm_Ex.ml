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
  let Vm__Vm.VMS (p, r, s, m) = ms in
  begin
    if Z.lt p Z.zero then begin raise Err end;
    begin try
      begin match split_at p c with
      | (_, instr :: _) ->
        begin match instr with
        | Vm__Vm.Iconst n ->
          Vm__Vm.VMS ((Z.add p Z.one), r, (Vm__Vm.push n s), m)
        | Vm__Vm.Ihalt -> raise (Halt ms)
        | Vm__Vm.Iimm (idr, n) ->
          Vm__Vm.VMS ((Z.add p Z.one), (State__Reg.write r idr n), s, m)
        | Vm__Vm.Iload (idr, id) ->
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r idr (State__State.mixfix_lbrb m id)), s, m)
        | Vm__Vm.Istore (idr, id) ->
          Vm__Vm.VMS ((Z.add p Z.one), r, s,
            (State__State.set m id (State__Reg.read r idr)))
        | Vm__Vm.Ipushr idr ->
          Vm__Vm.VMS ((Z.add p Z.one), r,
            (Vm__Vm.push (State__Reg.read r idr) s), m)
        | Vm__Vm.Ipopr idr ->
          let (popped, newS) = pop s in
          Vm__Vm.VMS ((Z.add p Z.one), (State__Reg.write r idr popped), newS,
            m)
        | Vm__Vm.Iaddr (term1, term2, sum) ->
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r sum
               (Z.add (State__Reg.read r term1) (State__Reg.read r term2))),
            s, m)
        | Vm__Vm.Iaddur (term1, term2, sum) ->
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r sum
               (Bv_op__BV_OP.bv_add (State__Reg.read r term1)
                  (State__Reg.read r term2))), s, m)
        | Vm__Vm.Isubr (term1, term2, diff) ->
          Vm__Vm.VMS ((Z.add p Z.one),
            (State__Reg.write r diff
               (Z.sub (State__Reg.read r term1) (State__Reg.read r term2))),
            s, m)
        | Vm__Vm.Ibeqr (idr1, idr2, ofs) ->
          let newOfs =
            begin match Z.equal (State__Reg.read r idr1) (State__Reg.read r
                                                            idr2) with
            | true -> ofs
            | false -> Z.zero
            end in
          Vm__Vm.VMS ((Z.add (Z.add p newOfs) Z.one), r, s, m)
        | Vm__Vm.Ibner (idr1, idr2, ofs) ->
          let newOfs =
            begin match not (Z.equal (State__Reg.read r idr1) (State__Reg.read r
                                                                 idr2)) with
            | true -> ofs
            | false -> Z.zero
            end in
          Vm__Vm.VMS ((Z.add (Z.add p newOfs) Z.one), r, s, m)
        | Vm__Vm.Ibler (idr1, idr2, ofs) ->
          let newOfs =
            begin match Z.leq (State__Reg.read r idr1) (State__Reg.read r
                                                          idr2) with
            | true -> ofs
            | false -> Z.zero
            end in
          Vm__Vm.VMS ((Z.add (Z.add p newOfs) Z.one), r, s, m)
        | Vm__Vm.Ibgtr (idr1, idr2, ofs) ->
          let newOfs =
            begin match Z.gt (State__Reg.read r idr1) (State__Reg.read r idr2) with
            | true -> ofs
            | false -> Z.zero
            end in
          Vm__Vm.VMS ((Z.add (Z.add p newOfs) Z.one), r, s, m)
        | _ -> raise Err
        end
      | _ -> assert false (* absurd *)
      end with
    | Err -> raise Err
    end
  end

let rec instr_iter_ex (c: Vm__Vm.instr list) (ms: Vm__Vm.machine_state) :
  Vm__Vm.machine_state = let o = instr_ex c ms in instr_iter_ex c o

