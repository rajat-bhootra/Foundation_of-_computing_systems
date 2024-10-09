(* parser.ml *)

(*open ast.ml file*)
open Ast

(* Helper function to preprocess individual lines, handling multi-line comments and single-line comments *)
let preprocess_line line =
  let line = String.trim line in
  if String.contains line '/' then
    if String.contains line '/' && String.contains line '*' then
      (* Handle potential multi-line comment start *)
      let comment_start = try String.index line '/' with Not_found -> String.length line in
      String.trim (String.sub line 0 comment_start)
    else if String.contains line '/' then
      (* Handle single-line comment *)
      let comment_start = try String.index line '/' with Not_found -> String.length line in
      String.trim (String.sub line 0 comment_start)
    else
      line  (* No comment found *)
  else
    line  (* No comment found *)

(* Create a recursive function to handle multi-line comments *)
let rec preprocess lines is_in_multiline_comment acc =
  match lines with
  | [] -> List.rev acc  (* Reverse the accumulated results *)
  | line :: rest ->
    if is_in_multiline_comment then
      (* If currently inside a multi-line comment, check for closing "*/" *)
      if String.contains line '*' && String.contains line '/' then
        let after_comment = String.sub line (String.index line '/' + 1) (String.length line - (String.index line '/' + 1)) in
        preprocess rest false (preprocess_line after_comment :: acc)
      else
        preprocess rest true acc  (* Continue skipping lines in the multi-line comment *)
    else if String.contains line '/' && String.contains line '*' then
      (* Start of a multi-line comment *)
      let before_comment = String.sub line 0 (String.index line '/') in
      preprocess rest true (preprocess_line before_comment :: acc)
    else
      preprocess rest false (preprocess_line line :: acc)  (* Process normal lines *)

(* Check if a line is empty *)
let is_empty_line line =
  line = "" || String.trim line = ""

(* Validate that the label follows allowed naming conventions *)
let is_valid_label label =
  let label_length = String.length label in
  let rec check i =
    if i >= label_length then true
    else
      match label.[i] with
      | 'a'..'z' | 'A'..'Z' | '0'..'9' | '.' | '_' | ':' | '$' -> check (i + 1)
      | _ -> false
  in
  (* A label cannot start with a digit *)
  if label_length > 0 && not (label.[0] >= '0' && label.[0] <= '9') then
    check 0
  else
    false
    
(* Parsing A-instructions of the form @123 or @symbol *)
let parse_a_instruction line =
  let value = String.sub line 1 (String.length line - 1) in
  if String.length value = 0 then
    failwith "Invalid A-instruction: empty label or address after @"
  else
    try
      (*if it is A-instruction of type @(any integer)*)
      let int_value = int_of_string value in
      AInstruction (AAddress int_value)
    with Failure _ ->
      (*if it is tyepe @(symbols)*)
      if is_valid_label value then
        AInstruction (ALabel value)
      else
        failwith ("Invalid A-instruction label: " ^ value)
        
(* Parsing L-instructions of the form (LABEL) *)
let parse_l_instruction line =
  let label = String.sub line 1 (String.length line - 2) in
  LInstruction label

(* Parsing dest, comp, and jump fields for C-instructions *)
let parse_dest mnemonic =
  match mnemonic with
  | "" -> NullDest
  | "M" -> M
  | "D" -> D
  | "MD" -> MD
  | "A" -> A
  | "AM" -> AM
  | "AD" -> AD
  | "AMD" -> AMD
  | _ -> failwith ("Invalid dest mnemonic: " ^ mnemonic)

let parse_comp mnemonic =
  match mnemonic with
  | "0" -> Zero
  | "1" -> One
  | "-1" -> NegOne
  | "D" -> DReg
  | "A" -> AReg
  | "M" -> MReg
  | "!D" -> NotD
  | "!A" -> NotA
  | "!M" -> NotM
  | "-D" -> NegD
  | "-A" -> NegA
  | "-M" -> NegM
  | "D+1" -> DPlus1
  | "A+1" -> APlus1
  | "M+1" -> MPlus1
  | "D-1" -> DMinus1
  | "A-1" -> AMinus1
  | "M-1" -> MMinus1
  | "D+A" -> DPlusA
  | "D+M" -> DPlusM
  | "D-A" -> DMinusA
  | "D-M" -> DMinusM
  | "A-D" -> AMinusD
  | "M-D" -> MMinusD
  | "D&A" -> DAndA
  | "D&M" -> DAndM
  | "D|A" -> DOrA
  | "D|M" -> DOrM
  | _ -> failwith ("Invalid comp mnemonic: " ^ mnemonic)

let parse_jump mnemonic =
  match mnemonic with
  | "" -> NullJump
  | "JGT" -> JGT
  | "JEQ" -> JEQ
  | "JGE" -> JGE
  | "JLT" -> JLT
  | "JNE" -> JNE
  | "JLE" -> JLE
  | "JMP" -> JMP
  | _ -> failwith ("Invalid jump mnemonic: " ^ mnemonic)

(* Parsing C-instructions of the form dest=comp;jump *)
let parse_c_instruction line =
  let dest_comp, jump =
    (*first split istruction from char ';'*)
    match String.split_on_char ';' line with
    | [d; j] -> (d, j)
    | [d] -> (d, "")
    | _ -> failwith "Invalid C-instruction format"
  in
  let dest, comp =
    (*now split instruction from char '='*)
    match String.split_on_char '=' dest_comp with
    | [d; c] -> (d, c)
    | [c] -> ("", c)
    | _ -> failwith "Invalid C-instruction format"
  in
  CInstruction {
    dest = parse_dest dest;
    comp = parse_comp comp;
    jump = parse_jump jump;
  }

(* Parse a single line of Hack assembly *)
let parse_line line =
  if String.length line = 0 then
    failwith "Attempted to parse an empty line"
  else if String.get line 0 = '@' then
    parse_a_instruction line
  else if String.get line 0 = '(' && String.get line (String.length line - 1) = ')' then
    parse_l_instruction line
  else
    parse_c_instruction line

(* Parse the entire program, applying preprocessing to remove comments and whitespace *)
let parse_program lines =
  preprocess lines false []  (* Start preprocessing without being in a multi-line comment *)
  |> List.filter (fun line -> not (is_empty_line line))  (* Remove empty lines *)
  |> List.map parse_line  (* Parse each remaining line *)
