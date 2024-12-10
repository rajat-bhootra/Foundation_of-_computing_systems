open Ast

(*convert string to corresponding segment type*)
let segment_of_string s =
  match String.lowercase_ascii s with
  | "constant" -> Some Constant
  | "local" -> Some Local
  | "argument" -> Some Argument
  | "this" -> Some This
  | "that" -> Some That
  | "temp" -> Some Temp
  | "pointer" -> Some Pointer
  | "static" -> Some Static
  | _ -> None

(* Safely convert a string to an integer option *)
let int_of_string_opt s =
  try Some (int_of_string s) with
  | Failure _ -> None

(* Helper function to remove comments from a line *)
let remove_inline_comment line =
  let trimmed_line = String.trim line in
  match String.index_opt trimmed_line '/' with
  | Some i when i < String.length trimmed_line - 1 && trimmed_line.[i + 1] = '/' ->
      (* Remove everything after the '//' *)
      String.trim (String.sub trimmed_line 0 i)
  | _ -> trimmed_line

(* Parse each line into an instruction *)
let parse_line line =
  let line_no_comment = remove_inline_comment line in
  (* Skip empty or comment-only lines *)
  if line_no_comment = "" then
    None
  else
    (* Split the line on spaces and parse tokens *)
    let tokens = String.split_on_char ' ' line_no_comment in
    match tokens with
    | ["push"; segment; value] ->
        (match segment_of_string segment, int_of_string_opt value with
         | Some seg, Some v -> Some (Push (seg, v))
         | _ -> None)
    | ["pop"; segment; value] ->
        (match segment_of_string segment, int_of_string_opt value with
         | Some seg, Some v -> Some (Pop (seg, v))
         | _ -> None)
    | ["add"] -> Some Add
    | ["sub"] -> Some Sub
    | ["neg"] -> Some Neg
    | ["eq"] -> Some Eq
    | ["gt"] -> Some Gt
    | ["lt"] -> Some Lt
    | ["and"] -> Some And
    | ["or"] -> Some Or
    | ["not"] -> Some Not
    | ["if-goto"; label] -> Some (IfGoto label)
    | ["goto"; label] -> Some (Goto label)
    | ["label"; label] -> Some (Label label)
    | ["call"; name; nargs] ->
        (match int_of_string_opt nargs with
         | Some n -> Some (Call (name, n))
         | None -> None)
    | ["function"; name; nlocals] ->
        (match int_of_string_opt nlocals with
         | Some n -> Some (Function (name, n))
         | None -> None)
    | ["return"] -> Some Return
    | _ -> Printf.printf "Unknown instruction: %s\n" line; None

