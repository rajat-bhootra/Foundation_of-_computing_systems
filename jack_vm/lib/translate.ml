(*open ast.ml*)
open Ast


(* Add a global counter for generating unique labels *)
let label_counter = ref 0  (* A global counter to generate unique labels *)

(* Generate a unique label based on the prefix and increment the counter *)
let unique_label prefix =
  let label = prefix ^ string_of_int !label_counter in
  label_counter := !label_counter + 1;
  label

(* Helper function to convert segment to string *)
let segment_to_string segment =
  match segment with
  | Constant -> "constant"
  | Local -> "local"
  | Argument -> "argument"
  | This -> "this"
  | That -> "that"
  | Temp -> "temp"
  | Pointer -> "pointer"
  | Static -> "static"

let translate_push segment index =
  match segment with
  | Constant -> Printf.sprintf "@%d\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1" index
  | Local -> Printf.sprintf "@LCL\nD=M\n@%d\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" index
  | Argument -> Printf.sprintf "@ARG\nD=M\n@%d\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" index
  | This -> Printf.sprintf "@THIS\nD=M\n@%d\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" index
  | That -> Printf.sprintf "@THAT\nD=M\n@%d\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" index
  | Temp -> Printf.sprintf "@R5\nD=A\n@%d\nA=D+A\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" index
  | Pointer -> Printf.sprintf "@R%d\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" (3 + index)
  | Static -> Printf.sprintf "@%s.%d\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1" "STATIC" index

(* Translate Pop command *)
let translate_pop segment index =
  match segment with
  | Constant -> failwith "Cannot pop to constant"
  | Local -> Printf.sprintf "@LCL\nD=M\n@%d\nD=D+A\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D" index
  | Argument -> Printf.sprintf "@ARG\nD=M\n@%d\nD=D+A\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D" index
  | This -> Printf.sprintf "@THIS\nD=M\n@%d\nD=D+A\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D" index
  | That -> Printf.sprintf "@THAT\nD=M\n@%d\nD=D+A\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D" index
  | Temp -> Printf.sprintf "@R5\nD=A\n@%d\nD=D+A\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D" index
  | Pointer -> Printf.sprintf "@R%d\nD=A\n@R13\nM=D\n@SP\nAM=M-1\nD=M\n@R13\nA=M\nM=D" (3 + index)
  | Static -> Printf.sprintf "@SP\nAM=M-1\nD=M\n@%s.%d\nM=D" "STATIC" index

(* Translate arithmetic/logical operations *)
let translate_arithmetic command =
  match command with
  | Add -> "@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nM=D+M\n@SP\nM=M+1"
  | Sub -> "@SP\nAM=M-1\nD=M\n@SP\nAM=M-1\nM=M-D\n@SP\nM=M+1"
  | Neg -> "@SP\nA=M-1\nD=M\nM=-D"
  | Eq  ->
      let label_true = unique_label "EQ_TRUE" in 
      let label_end = unique_label "EQ_END" in
      Printf.sprintf
        "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@%s\nD;JEQ\n@SP\nA=M-1\nM=0\n@%s\n0;JMP\n(%s)\n@SP\nA=M-1\nM=-1\n(%s)"
        label_true label_end label_true label_end
  | Gt ->
      let label_true = unique_label "GT_TRUE" in 
      let label_end = unique_label  "GT_END" in
      Printf.sprintf
        "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@%s\nD;JGT\n@SP\nA=M-1\nM=0\n@%s\n0;JMP\n(%s)\n@SP\nA=M-1\nM=-1\n(%s)"
        label_true label_end label_true label_end
  | Lt ->
      let label_true = unique_label "LT_TRUE" in 
      let label_end = unique_label "LT_END" in
      Printf.sprintf
        "@SP\nAM=M-1\nD=M\nA=A-1\nD=M-D\n@%s\nD;JLT\n@SP\nA=M-1\nM=0\n@%s\n0;JMP\n(%s)\n@SP\nA=M-1\nM=-1\n(%s)"
        label_true label_end label_true label_end
  | And -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M&D"
  | Or -> "@SP\nAM=M-1\nD=M\nA=A-1\nM=M|D"
  | Not -> "@SP\nA=M-1\nM=!M"
  | _ -> failwith "Unexpected non-arithmetic/logical command"

(* Translate branching commands*)
let translate_label label = Printf.sprintf "(%s)" label
let translate_if_goto label = Printf.sprintf "@SP\nAM=M-1\nD=M\n@%s\nD;JNE" label
let translate_goto label = Printf.sprintf "@%s\n0;JMP" label


(*counter to check repetation of labels*)
let return_label_counter = ref 0

(* Translate function call commands*)
let translate_call function_name num_args =
  let return_label = Printf.sprintf "RETURN_%s_%d" function_name !return_label_counter in
  return_label_counter := !return_label_counter + 1;
  Printf.sprintf
    "@%s\nD=A\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\
     @LCL\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\
     @ARG\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\
     @THIS\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\
     @THAT\nD=M\n@SP\nA=M\nM=D\n@SP\nM=M+1\n\
     @SP\nD=M\n@5\nD=D-A\n@%d\nD=D-A\n@ARG\nM=D\n\
     @SP\nD=M\n@LCL\nM=D\n\
     @%s\n0;JMP\n(%s)"
    return_label num_args function_name return_label

(*Translate function command*)
let translate_function function_name num_locals =
  let init_locals = String.concat "\n" (List.init num_locals (fun _ -> "@SP\nA=M\nM=0\n@SP\nM=M+1")) in
  Printf.sprintf "(%s)\n%s" function_name init_locals

(*Translate function return commands*)
let translate_return =
  "@LCL\nD=M\n@R13\nM=D\n\
   @5\nA=D-A\nD=M\n@R14\nM=D\n\
   @SP\nAM=M-1\nD=M\n@ARG\nA=M\nM=D\n\
   @ARG\nD=M+1\n@SP\nM=D\n\
   @R13\nAM=M-1\nD=M\n@THAT\nM=D\n\
   @R13\nAM=M-1\nD=M\n@THIS\nM=D\n\
   @R13\nAM=M-1\nD=M\n@ARG\nM=D\n\
   @R13\nAM=M-1\nD=M\n@LCL\nM=D\n\
   @R14\nA=M\n0;JMP"

(* The main translate function *)
let translate_instruction  = function
  | Push (segment, index) -> translate_push segment index
  | Pop (segment, index) -> translate_pop segment index
  | Add | Sub | Neg | Eq | Gt | Lt | And | Or | Not as arith -> translate_arithmetic arith
  | IfGoto label -> translate_if_goto label
  | Goto label -> translate_goto label
  | Label label -> translate_label label
  | Call (function_name, num_args) -> translate_call function_name num_args
  | Function (function_name, num_locals) -> translate_function function_name num_locals
  | Return -> translate_return

(*initate stack pointer and call Sys.init*)
let bootstrap = "@256\nD=A\n@SP\nM=D\n@Sys.init\n0;JMP\n"

