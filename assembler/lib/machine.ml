(* machine.ml *)

(*open ast.ml*)
open Ast

(* Convert 'dest' instruction to its binary representation *)
let dest_to_bin = function
  | NullDest -> "000"
  | M        -> "001"
  | D        -> "010"
  | MD       -> "011"
  | A        -> "100"
  | AM       -> "101"
  | AD       -> "110"
  | AMD      -> "111"

(* Convert 'comp' instruction to its binary representation *)
let comp_to_bin = function
  | Zero    -> "0101010"
  | One     -> "0111111"
  | NegOne  -> "0111010"
  | DReg    -> "0001100"
  | AReg    -> "0110000"
  | MReg    -> "1110000"
  | NotD    -> "0001101"
  | NotA    -> "0110001"
  | NotM    -> "1110001"
  | NegD    -> "0001111"
  | NegA    -> "0110011"
  | NegM    -> "1110011"
  | DPlus1  -> "0011111"
  | APlus1  -> "0110111"
  | MPlus1  -> "1110111"
  | DMinus1 -> "0001110"
  | AMinus1 -> "0110010"
  | MMinus1 -> "1110010"
  | DPlusA  -> "0000010"
  | DPlusM  -> "1000010"
  | DMinusA -> "0010011"
  | DMinusM -> "1010011"
  | AMinusD -> "0000111"
  | MMinusD -> "1000111"
  | DAndA   -> "0000000"
  | DAndM   -> "1000000"
  | DOrA    -> "0010101"
  | DOrM    -> "1010101"

(* Convert 'jump' instruction to its binary representation *)
let jump_to_bin = function
  | NullJump -> "000"
  | JGT      -> "001"
  | JEQ      -> "010"
  | JGE      -> "011"
  | JLT      -> "100"
  | JNE      -> "101"
  | JLE      -> "110"
  | JMP      -> "111"

(* Define a function to convert an integer to a 15-bit binary string *)
let int_to_15bit_binary n =
  let rec to_binary n =
    if n = 0 then ""
    else to_binary (n / 2) ^ string_of_int (n mod 2)
  in
  let binary = to_binary n in
  let padding = String.make (15 - String.length binary) '0' in
  padding ^ binary

(* Define a function to convert an 'a_instruction' to its binary representation *)
let a_instruction_to_bin = function
  (*if it is an integer*)
  | AAddress value -> "0" ^ int_to_15bit_binary value
  (*if it is a symbol*)
  | ALabel _ -> failwith "ALabel should be resolved to an address before machine code conversion"

(* Define a function to convert a 'c_instruction' to its binary representation *)
let c_instruction_to_bin c_instr =
   (*get its comp,dest and jump binary representation*)
  "111" ^ (comp_to_bin c_instr.comp) ^ (dest_to_bin c_instr.dest) ^ (jump_to_bin c_instr.jump)

(* Define a funtion to convert a single instruction to its binary representation *)
let instruction_to_bin = function
  | AInstruction a_instr -> a_instruction_to_bin a_instr
  | CInstruction c_instr -> c_instruction_to_bin c_instr
  | LInstruction _ -> failwith "LInstruction should be resolved and removed before machine code conversion"

(* Convert a list of instructions to machine code (each instruction as a line) *)
let instructions_to_machine_code instructions =
  List.map instruction_to_bin instructions

(*Convert a program to machine code*)
 let assemble_program program =
     let binary_instructions = instructions_to_machine_code program in
     String.concat "\n" binary_instructions
