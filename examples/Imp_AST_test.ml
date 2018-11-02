module Imp = Imp__Imp
module Vm = Vm__Vm
module State = State__State
module Compile = Compiler__Compile_com
module Imp_Ex = Imp_ex__Imp_Ex
module Vm_Ex = Vm_ex__Vm_Ex

open State
open Imp
open Vm
open Dump
open Compile
open Imp_Ex
open Common

let () =
  p_stdout ("Imp_AST_test" ^ nl);
  (* setup some varibles *)
  let v1 = Env.add_id ("v1") in (* variable with index 1 *)
  let v2 = Env.add_id ("v2") in (* variable with index 2 *)

  let a1 = Anum (Z.of_int 1) in
  let a2 = Anum (Z.of_int 2) in
  let av1 = Avar v1 in
  let av2 = Avar v2 in
  let a = Asub(Aadd (a1, a2), Aadd(av1, av2)) in
  Printf.printf "a : %s\n" (of_aexpr a);

  let c1 = Cassign (v1, a1) in
  let c2 = Cassign (v2, a2) in
  let c3 = Cassign (v2, a) in
  let c = Cseq (c1, Cseq(c2, c3)) in

  p_stdout ("commands :" ^ nl ^ pretty_of_com 0 c ^ nl);

  let st_0 : state = State.const (Z.of_int 0) in
  p_stdout ("st_0" ^ nl ^ Env.to_string st_0 ^ nl);

  let st_end = Imp_Ex.ceval_ex st_0 c in
  p_stdout ("ceval_ex" ^ nl ^ Env.to_string st_end ^ nl);


  let p = compile_program c in
  p_stdout ("instructions: " ^ nl ^ of_code true p ^ nl);

  try
    let _ = Vm_Ex.instr_iter_ex p (VMS (Z.of_int 0, [], st_0)) in
    ()
  with
  | Vm_Ex.Err -> p_stderr ("execution error")
  | Vm_Ex.Halt (VMS (pos, stack, st_halt)) ->
    p_stdout ("execution halted");
    p_stdout ("instr_iter_ex" ^ nl ^ Env.to_string st_halt ^ nl);
    ()




