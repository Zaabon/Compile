open Imp__Imp
open State__State
open Vm__Vm
open Env
open Common
open Printf
  
let file = "../MIPS/instructions.s"
let startString = ".text           # .text segment (code)
		.set noreorder	# necessary to avoid code optimization\n"
let endString = "\nstop:		b stop
		
                .data   # .data segment
res_heap:       .word 0x00000000
bytearr_heap:	.byte 0
                .byte 1
                .byte 2
                .byte 3
word_heap:      .word 0x00010203"  

let writeThis = function
  | instructions ->
  let content = startString^instructions^endString in
  (* Write message to file *)
  let oc = open_out file in    (* create or truncate file, return channel *)
  fprintf oc "%s\n" content;   (* write something *)   
  close_out oc;                (* flush and close the channel *)