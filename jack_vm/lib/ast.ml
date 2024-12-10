(* Define the segment type *)
type segment =
  | Constant
  | Argument
  | Local
  | Static
  | This
  | That
  | Pointer
  | Temp

(* Define an abstract instruction type *)
type 'a instruction =
  | Push of segment * int
  | Pop of segment * int
  | Add
  | Sub
  | Neg
  | Eq
  | Gt
  | Lt
  | And
  | Or
  | Not
  | Label of string 
  | Goto of string
  | IfGoto of string  
  | Function of string * int
  | Call of string * int
  | Return
