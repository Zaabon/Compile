module BV32 = Bv_op__BV_OP

let () =
  Printf.printf "BV32_test\n";
  (* setup some varibles *)
  let v = BV32.bv_add (Z.of_int 0x7fff_ffff) (Z.of_int 1) in
    Printf.printf "a : %s\n" (Z.to_string v);
  ()