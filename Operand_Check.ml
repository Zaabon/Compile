open T_Check

let rec operand_type_check operand ch itl (a1, a1_span) (a2, a2_span) =
    let (ai1, t1) = T_Check.tc_aexpr ch itl (a1, a1_span) in
    let _ = tc_unify ch Tsint t1 a1_span in

    (* type check a2 against Tsint *)
    let (ai2, t2) = T_Check.tc_aexpr ch itl (a2, a2_span) in
    let _ = tc_unify ch Tsint t2 a2_span in
    match operand with 
      | Aadd ((a1, a1_span), (a2, a2_span)) -> (Imp.Aadd(ai1, ai2), Tsint)
      | Aaddu ((a1, a1_span), (a2, a2_span)) -> (Imp.Aaddu(ai1, ai2), Tuint32)
      | Asub ((a1, a1_span), (a2, a2_span)) -> (Imp.Asub(ai1, ai2), Tsint)
