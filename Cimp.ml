(* Copyright Per Lindgren 2018, see the file "LICENSE" *)
(* for the full license governing this code.           *)

(* local files *)
open Common 

(* extracted code (/extract) *)
module Compile = Compiler__Compile_com
module Compile_Reg = Compiler__Compile_com_reg
module Vm_Ex = Vm_ex_assignment__Vm_Ex
module Imp_Ex = Imp_ex_assignment__Imp_ex
module State = State__State
module Reg = State__Reg
module Optimize = Ast_opt__AST_opt

module Imp = Imp__Imp

let () =
  Cmd.cmd; (* parse command line options and put into opt *)
  p_stderr (Options.string_of_opt Options.opt);
  try
    let inBuffer = open_in Options.opt.infile in
    let lexbuf = Lexing.from_channel inBuffer in
    lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = Options.opt.infile };
    try
      let p = Parser.prog Lexer.lex lexbuf in
      (* (* Comment out to get a Dump of the T_Imp with (some) span information *)

         p_stdout ("Decl:" ^ nl ^ T_Dump.of_prog p);  
      *)
        
    (*  if Options.opt.optimize then
        let com = Optimize.com_opt com  in 
      *)  

      let com = T_Check.tc_prog inBuffer p in
      let com = match Options.opt.optimize with
        | true  -> Optimize.com_opt com
        | false -> com
      in
      
      if Options.opt.d_ast then
        p_stderr ("Raw AST:" ^ nl ^ Dump.of_com com ^ nl);
      if Options.opt.d_past then
        p_stderr ("Pretty AST:" ^ nl ^ Dump.pretty_of_com 0 com ^ nl);

      let code = 
        if Options.opt.reg then 
          Compile_Reg.compile_program com
        else
          Compile.compile_program com
      in
      let instructions = MIPS.of_code false 0 code in
      if Options.opt.reg then (
        WriteFile.writeThis instructions;
        p_stderr ("Reg : \n" ^ instructions ^ nl)
      );

      if Options.opt.d_code then
          p_stderr ("Raw Code : \n" ^ Dump.of_code false code ^ nl);
      if Options.opt.d_pcode then
          p_stderr ("Pretty Code : \n" ^ Dump.of_code true code ^ nl);


      let st_0 = State.const (Z.of_int 0) in (* assume all variables 0 *)

      (* imp_ex execution *)
      if Options.opt.imp_ex then (
        try
          p_stdout ("Execute : imp_ex" );
          let st_end = Imp_Ex.ceval_ex st_0 com in
          p_stdout ("ceval_ex" ^ nl ^ Env.to_string st_end ^ nl);
        with
        | _ -> p_stdout "ceval : Exited with an error\n";
      );

      (* vm_ex execution *)
      if Options.opt.vm_ex then (
        try
          let _ = Vm_Ex.instr_iter_ex code (VMS (Z.of_int 0, Reg.const(Z.of_int 0), [], st_0)) in
          ()
        with
        | Vm_Ex.Err -> p_stderr ("execution error")
        | Vm_Ex.Halt (VMS (pos, reg, stack, st_halt)) ->
          p_stdout ("execution halted");
          p_stdout ("instr_iter_ex" ^ nl ^ Env.to_string st_halt ^ nl);
          ()
      );  

      p_stdout ("Done!");

    with
    | Lexer.SyntaxError msg -> 
      raise (CompilerError ("Syntax error. " ^ msg ^ Error.parse_err_msg inBuffer lexbuf));
    | Parser.Error -> 
      raise (CompilerError ("Parser error." ^ Error.parse_err_msg inBuffer lexbuf));

  with
  | CompilerError msg -> p_stderr msg;
    exit (-1);
  | Failure msg -> p_stderr ("Failure (internal error): " ^ msg);
    exit (-1);
  | Sys_error msg -> p_stderr("File open error :" ^ Options.opt.infile ^ msg);
    exit (-1);;

