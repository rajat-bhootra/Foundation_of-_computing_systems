(* Helper function to read lines from a file *)
let read_lines filename =
  let ic = open_in filename in
  let rec read_all_lines acc =
    match input_line ic with
    | line -> read_all_lines (line :: acc)
    | exception End_of_file -> close_in ic; List.rev acc
  in
  read_all_lines []

let parse_instruction = Jack_vm.Parser.parse_line

(* Main program logic *)
let () =
  if Array.length Sys.argv < 2 then
    Printf.printf "Usage: %s <vm_file.vm>\n" Sys.argv.(0)
  else
    let filename = Sys.argv.(1) in
    try
      let lines = read_lines filename in
      (* Parse and filter out None values from the input lines *)
      let instructions = List.filter_map parse_instruction lines in
      (* Translate the parsed instructions to Hack assembly *)
      let hack_assembly = List.map Jack_vm.Translate.translate_instruction instructions in
      let final_asm = [Jack_vm.Translate.bootstrap] @ hack_assembly in

      (* Optional: Save the output to an .asm file instead of printing to the console *)
      let asm_filename = (Filename.remove_extension filename) ^ ".asm" in
      let oc = open_out asm_filename in
      List.iter (fun line -> output_string oc (line ^ "\n")) final_asm;
      close_out oc;

      Printf.printf "Translation successful! Output written to %s\n" asm_filename
    with
    | Sys_error msg -> Printf.printf "Error: %s\n" msg
    | Failure msg -> Printf.printf "Parsing or translation error: %s\n" msg

