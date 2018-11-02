let rec aexpr_opt (e: Imp__Imp.aexpr) : Imp__Imp.aexpr =
  begin match e with
  | Imp__Imp.Aaddu (e1, e2) ->
    begin match (aexpr_opt e1, aexpr_opt e2) with
    | (Imp__Imp.Anum n1, Imp__Imp.Anum n2) ->
      Imp__Imp.Anum (Bv_op__BV_OP.bv_add n1 n2)
    | (e3, e4) -> Imp__Imp.Aaddu (e3, e4)
    end
  | Imp__Imp.Asubu (e1, e2) ->
    if e1 = e2 then begin Imp__Imp.Anum Z.zero end
    else
    begin
      begin match (aexpr_opt e1, aexpr_opt e2) with
      | (Imp__Imp.Anum n1, Imp__Imp.Anum n2) ->
        Imp__Imp.Anum (Bv_op__BV_OP.bv_sub n1 n2)
      | (e3, e4) ->
        if e3 = e4 then begin Imp__Imp.Anum Z.zero end
        else
        begin
          Imp__Imp.Asubu (e3, e4) end
      end end
  | _ -> e
  end

let rec bexpr_opt (e: Imp__Imp.bexpr) : Imp__Imp.bexpr =
  begin match e with
  | Imp__Imp.Band (e1, e2) ->
    begin match (bexpr_opt e1, bexpr_opt e2) with
    | (_, Imp__Imp.Bfalse) -> Imp__Imp.Bfalse
    | (Imp__Imp.Bfalse, _) -> Imp__Imp.Bfalse
    | (a, Imp__Imp.Btrue) -> a
    | (Imp__Imp.Btrue, a) -> a
    | (Imp__Imp.Btrue, Imp__Imp.Btrue) -> Imp__Imp.Btrue
    | (e3, e4) ->
      if e1 = e2 then begin e3 end else begin Imp__Imp.Band (e3, e4) end
    end
  | _ -> e
  end

let rec com_opt (c: Imp__Imp.com) : Imp__Imp.com =
  begin match c with
  | Imp__Imp.Cif (b, c1, c2) ->
    let bo = bexpr_opt b in
    begin match bo with
    | Imp__Imp.Btrue -> com_opt c1
    | Imp__Imp.Bfalse -> com_opt c2
    | _ -> Imp__Imp.Cif (bo, (com_opt c1), (com_opt c2))
    end
  | Imp__Imp.Cwhile (b, c1) ->
    let bo = bexpr_opt b in
    begin match bo with
    | Imp__Imp.Bfalse -> Imp__Imp.Cskip
    | _ -> Imp__Imp.Cwhile (bo, c1)
    end
  | Imp__Imp.Cassign (id, a) -> Imp__Imp.Cassign (id, (aexpr_opt a))
  | Imp__Imp.Cseq (c1, c2) -> Imp__Imp.Cseq ((com_opt c1), (com_opt c2))
  | _ -> c
  end

