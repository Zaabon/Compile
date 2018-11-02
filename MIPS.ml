(* Copyright Per Lindgren 2016-2018, see the file "LICENSE" *)
(* for the full license governing this code.                *)

open Imp__Imp
open State__State
open Vm__Vm
open Env
open Common

(* AST dump of native data structures *)
let of_id = function
  | Id i -> string_of_int (4 * int_of_string (Z.to_string i))

let move_sp = function
  | i -> let ofs = (i*4) in "\tADDI \t$sp, $sp, "^string_of_int ofs^"\n"

let pop reg1 reg2 ofs = "\tLW \t"^reg1^", 0($sp)\n"^
                        "\tLW \t"^reg2^", 4($sp)\n"^
                        move_sp ofs

let pc_ofs n pc = string_of_int (pc+ Z.to_int n)

let of_idr = function
  | idr -> ""^string_of_int (9 + Z.to_int idr)

let of_instr b pc= function
  | Iconst v        -> "\tADDI \t$t0, $0, "^Z.to_string v^"\n"^
                       move_sp (-1) ^
                       "\tSW \t$t0, 0($sp)"
  | Ivar id         -> "\tLW \t$t0, "^ of_id id ^"($gp)\n"^
                       move_sp (-1) ^
                       "\tSW \t$t0, 0($sp)"
  | Isetvar id      -> "\tLW \t$t0, 0($sp)\n"^
                       move_sp 1^
                       "\tSW \t$t0, "^ of_id id ^"($gp)"
  | Ibranch n       -> "\tJ \tL"^(pc_ofs (Z.of_int ((Z.to_int n))) (pc+1))
  | Iadd            -> (pop "$t0" "$t1" 1)^
                       "\tADD \t$t0, $t0, $t1\n"^
                       "\tSW \t$t0, 0($sp)"
  | Iaddu           -> (pop "$t0" "$t1" 1)^
                       "\tADDU \t$t0, $t0, $t1\n"^
                       "\tSW \t$t0, 0($sp)"
  | Isub            -> (pop "$t0" "$t1" 1)^
                       "\tSUB \t$t0, $t1, $t0\n"^
                       "\tSW \t$t0, 0($sp)"
  | Isubu           -> (pop "$t0" "$t1" 1)^
                       "\tSUBU \t$t0, $t1, $t0\n"^
                       "\tSW \t$t0, 0($sp)"
  | Ibeq n          -> (pop "$t0" "$t1" 2)^
                       "\tBEQ \t$t0, $t1, L"^ (pc_ofs n (pc+1))
  | Ibne n          -> (pop "$t0" "$t1" 2)^
                       "\tBNE \t$t0, $t1, L"^ (pc_ofs n (pc+1))
  | Ible n          -> (pop "$t0" "$t1" 2)^
                       "\tSUB \t$t0, $t0, $t1\n"^
                       "\tBLEZ \t$t0, L"^ (pc_ofs n (pc+1))
  | Ibgt n          -> (pop "$t0" "$t1" 2)^
                       "\tSUB \t$t0, $t0, $t1\n"^
                       "\tBGTZ \t$t0, L"^ (pc_ofs n (pc+1))
  | Ihalt           -> "\tJ \tL"^ (pc_ofs Z.zero pc)
  | Iload (idr, id) -> "\tLW \t$t0, "^ of_id id ^"($gp)\n"^
                       "\tADD \t$"^ of_idr idr ^", $0, $t0"
  | Iimm (idr, n)   -> "\tADDI \t$"^ of_idr idr ^", $0, "^ Z.to_string n 
  | Istore (idr, id)-> "\tSW \t$"^ of_idr idr ^", "^ of_id id ^"($gp)"
  | Ipushr (idr)    -> move_sp (-1) ^
                       "\tSW \t$"^ of_idr idr ^", 0($sp)"
  | Ipopr (idr)     -> "\tLW \t$"^ of_idr idr ^", 0($sp)\n"^
                       "\tADDI \t$sp, $sp, "^string_of_int 4
  | Iaddr (term1, term2, sum)   -> "\tADD \t$"^ of_idr sum ^", $"^ of_idr term1 ^", $"^ of_idr term2
  | Iaddur (term1, term2, sum)  -> "\tADDU \t$"^ of_idr sum ^", $"^ of_idr term1 ^", $"^ of_idr term2
  | Isubr (term1, term2, dif)   -> "\tSUB \t$"^ of_idr dif ^", $"^ of_idr term1 ^", $"^ of_idr term2
  | Isubur (term1, term2, dif)   -> "\tSUBU \t$"^ of_idr dif ^", $"^ of_idr term1 ^", $"^ of_idr term2
  | Ibeqr(idr1, idr2, ofs)        -> "\tBEQ \t$"^ of_idr idr1 ^", $"^ of_idr idr2 ^", "^ (pc_ofs ofs (pc))
  | Ibner (idr1, idr2, ofs)       -> "\tBNE \t$"^ of_idr idr1 ^", $"^ of_idr idr2 ^", "^ (pc_ofs ofs (pc+1))
  | Ibler (idr1, idr2, ofs)       -> "\tSUB \t$t0, $"^ of_idr idr1 ^", $"^ of_idr idr2 ^"\n"^
                                     "\tBLEZ \t$t0, L"^ (pc_ofs ofs (pc+1))
  | Ibgtr (idr1, idr2, ofs)       -> "\tSUB \t$t0, $"^ of_idr idr1 ^", $"^ of_idr idr2 ^"\n"^
                                     "\tBGTZ \t$t0, L"^ (pc_ofs ofs (pc+1))



let rec of_code b pc = function
  | [] -> ""
  | i :: il -> let newPc = pc+1 in
                "L"^ string_of_int pc ^ ": " ^ of_instr b pc i ^ "\n" ^ of_code b newPc il

