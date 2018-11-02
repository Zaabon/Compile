(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

(* type checker *)
open T_Imp
open T_Dump
open Common
open State__State


module Imp = Imp__Imp

(* converting a span to a string *)
let of_span inb (start, stop) = 
  let _ = seek_in inb start in
  let s = really_input_string inb (stop - start) in
  "<" ^ string_of_int start ^ ".." ^ string_of_int stop ^ "> " ^ s

(* report a duplicate definition *)
let unique_id chan (id1, (t1, s1)) (id2, (t2, s2)) = 
  if id1 = id2 then 
    raise (CompilerError("Duplicate variable definition: " ^
                         of_span chan s1 ^ " already declared at " ^  of_span chan s2)) 
  else ()

(* build a type environment in acc *)
let rec idt_acc ch sp acc = function 
  | Dseq (d1, d2) -> 
    idt_acc_span ch (idt_acc_span ch acc d1) d2 
  | Ddecl (id, t) -> 
    (* check that the identifier is not yet declared *)
    List.iter (unique_id ch (id, (t, sp))) acc; 
    (* add the identifier to acc *)
    (id, (t, sp)) :: acc
and idt_acc_span c acc (d, s) = idt_acc c s acc d

let of_idt ch (id, (t,s)) =
  of_id id ^ ":" ^ of_types t ^ of_span ch s

(* unify t2 to be compatible to the expected type t1 *)
let tc_unify ch t1 t2 s2 : types =
  match t1, t2 with
  | Tsint,    Tsint     -> Tsint
  | Tuint32,  Tuint32   -> Tuint32
  | Tint,     t         -> t
  | t,        Tint      -> t
  | _,        _         ->
    raise (TypeError(
        "Type error: Expected " ^  of_types t1 ^ 
        " got "  ^ of_types t2 ^ " in:" ^ of_span ch s2))

(* unify types t1 and t2 *)
let tc_unify2 ch t1 s1 t2 s2 : types =
  match t1, t2 with
  | Tsint,    Tsint     -> Tsint
  | Tuint32,  Tuint32   -> Tuint32
  | Tint,     t         -> t
  | t,        Tint      -> t
  | _,        _         ->
    raise (TypeError(
        "Type error: " ^ of_types t1 ^ " in:" ^ of_span ch s1 ^
        " does not match " ^ of_types t2 ^ " in:" ^ of_span ch s2))

(* lookup of identifier id in the type environment itl *)
let get_id_type itl (id : id) : types * span =
  try
    List.assoc id itl
  with 
    _ -> raise (TypeError("Undeclared identifier: " ^ Dump.pretty_of_id id))

(*
let rec operand_type_check ch itl (a1, a1_span) (a2, a2_span) =
  match a1 with
  | (ai1, t1) -> tc_aexpr ch itl (a1, a1_span)
*)

(*
let rec operand_type_check operand ch itl (a1, a1_span) (a2, a2_span) =
    (* type check a1 against Tsint *)
    let (ai1, t1) = tc_aexpr ch itl (a1, a1_span) in
    let _ = tc_unify ch Tsint t1 a1_span in (*!!!!! *)

    (* type check a2 against Tsint *)
    let (ai2, t2) = tc_aexpr ch itl (a2, a2_span) in
    let _ = tc_unify ch Tsint t2 a2_span in
    match operand with 
      | Aadd ((a1, a1_span), (a2, a2_span)) -> (Imp.Aadd(ai1, ai2), Tsint)
      | Aaddu ((a1, a1_span), (a2, a2_span)) -> (Imp.Aaddu(ai1, ai2), Tuint32)
      | Asub ((a1, a1_span), (a2, a2_span)) -> (Imp.Asub(ai1, ai2), Tsint)
*)

let rec tc_aexpr ch itl (a, span) : Imp.aexpr * types =  
  (* try *)
  match a with
  | Anum n -> (Imp.Anum n, Tint)
  | Acast (id, t) -> 
    let (_, _ ) = get_id_type itl id in
    (Imp.Avar id, t)
  | Avar id -> 
    let (t, _ ) = get_id_type itl id in
    (Imp.Avar id, t)
  | Aadd ((a1, a1_span), (a2, a2_span)) -> 
    let (ai1, ai2, t) = operand_type_check Tsint ch itl (a1, a1_span) (a2, a2_span) in
    (Imp.Aadd(ai1, ai2), t)
  | Aaddu ((a1, a1_span), (a2, a2_span)) -> 
    let (ai1, ai2, t) = operand_type_check Tuint32 ch itl (a1, a1_span) (a2, a2_span) in
    (Imp.Aaddu(ai1, ai2), t)

  | Asub ((a1, a1_span), (a2, a2_span)) -> 
    let (ai1, ai2, t) = operand_type_check Tsint ch itl (a1, a1_span) (a2, a2_span) in
    (Imp.Asub(ai1, ai2), t)
  | Asubu ((a1, a1_span), (a2, a2_span)) -> 
    let (ai1, ai2, t) = operand_type_check Tuint32 ch itl (a1, a1_span) (a2, a2_span) in
    (Imp.Asubu(ai1, ai2), t)

  (*
    | a -> let (a1, a1_span), (a2, a2_span), t = 
        match a with 
          | Aadd ((a1, a1_span), (a2, a2_span)) -> (a1, a1_span), (a2, a2_span), Tsint
          | Aaddu ((a1, a1_span), (a2, a2_span)) -> (a1, a1_span), (a2, a2_span), Tuint32
          | Asub ((a1, a1_span), (a2, a2_span)) -> (a1, a1_span), (a2, a2_span), Tsint
          | _ -> failwith "not possible"
        in

        (* type check a1 *)
        let (ai1, t1) = tc_aexpr ch itl (a1, a1_span) in
        let _ = tc_unify ch t t1 a1_span in 

        (* type check a2 *)
        let (ai2, t2) = tc_aexpr ch itl (a2, a2_span) in
        let _ = tc_unify ch t t2 a2_span in

        match a with 
          | Aadd ((a1, a1_span), (a2, a2_span)) -> (Imp.Aadd(ai1, ai2), t)
          | Aaddu ((a1, a1_span), (a2, a2_span)) -> (Imp.Aaddu(ai1, ai2), t)
          | Asub ((a1, a1_span), (a2, a2_span)) -> (Imp.Asub(ai1, ai2), t)
          | _ -> failwith "not possible"*)
and
operand_type_check t ch itl (a1, a1_span) (a2, a2_span) =
    (* type check a1 against Tsint *)
    let (ai1, t1) = tc_aexpr ch itl (a1, a1_span) in
    let _ = tc_unify ch t t1 a1_span in (*!!!!! *)

    (* type check a2 against Tsint *)
    let (ai2, t2) = tc_aexpr ch itl (a2, a2_span) in
    let _ = tc_unify ch t t2 a2_span in
    (ai1, ai2, t)

(*
    match operand with 
      | Aadd ((a1, a1_span), (a2, a2_span)) -> (Imp.Aadd(ai1, ai2), Tsint)
      | Aaddu ((a1, a1_span), (a2, a2_span)) -> (Imp.Aaddu(ai1, ai2), Tuint32)
      | Asub ((a1, a1_span), (a2, a2_span)) -> (Imp.Asub(ai1, ai2), Tsint)
*)

(* with
   | TypeError msg -> raise (TypeError (msg ^ nl ^ "in expression:" ^ of_span ch span )) *)

  

let rec tc_bexpr ch itl (b, span) = 
  try
    match b with
    | Btrue -> Imp.Btrue
    | Bfalse -> Imp.Bfalse
    | Band (b1, b2) -> Imp.Band(tc_bexpr ch itl b1,tc_bexpr ch itl b2)
    | Bnot b -> Imp.Bnot(tc_bexpr ch itl b)
    | Beq ((a1, a1_span),(a2, a2_span)) ->
      let (a1, t1) = tc_aexpr ch itl (a1, a1_span) in
      let (a2, t2) = tc_aexpr ch itl (a2, a2_span) in
      let _ = tc_unify2 ch t1 a1_span t2 a2_span in
      Imp.Beq(a1, a2)
    | Ble ((a1,a1_span), (a2, a2_span)) -> 
      (* type check a1 against Tsint *) 
      let (a1, t1) = tc_aexpr ch itl (a1, a1_span) in
      let _ = tc_unify ch Tsint t1 a1_span in

      (* type check a2 against Tsint *) 
      let (a2, t2) = tc_aexpr ch itl (a2, a2_span) in
      let _ = tc_unify ch Tsint t2 a2_span in
      Imp.Ble(a1,  a2)
  with
  | TypeError msg -> raise (TypeError (msg ^ nl ^ "in expression:" ^ of_span ch span ))




let rec tc_com ch itl span com = 
  try 
    match com with
    | Cseq (c1, c2) -> Imp.Cseq(tc_com_span ch itl c1, tc_com_span ch itl c2)
    | Cassign (id, a) ->
      let (_, a_span) = a in
      let (a, ta) = tc_aexpr ch itl a in
      let (tid, tid_span) = get_id_type itl id in
      let _ = tc_unify2 ch ta a_span tid tid_span in 
      Imp.Cassign (id, a)
    | Cif (b, c1, c2)  -> 
      let (b) = tc_bexpr ch itl b in
      let (c1) = tc_com_span ch itl c1 in
      let (c2) = tc_com_span ch itl c2 in
      Cif (b, c1, c2)
    | Cwhile (b, c) -> 
      let (b) = tc_bexpr ch itl b in
      let (c) = tc_com_span ch itl c in
      Cwhile(b, c)
    | Cskip -> Imp.Cskip
  with
  | TypeError msg -> raise (CompilerError (msg ^ nl ^ "in command: " ^ of_span ch span ))
and
  tc_com_span ch itl (com, span)  = tc_com ch itl span com

let tc_prog ch (Prog (decl, com)) = 
  let itl = idt_acc_span ch [] decl in
  tc_com_span ch itl com 

