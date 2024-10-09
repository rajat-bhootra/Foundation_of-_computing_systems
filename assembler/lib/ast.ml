(*ast.ml*)

(* Create a type defining A-type instruction *)
type a_instruction =
  | AAddress of int  
  | ALabel of string   

(* Create a type defining C-type instruction *)
type dest =
  | NullDest         
  | M                  
  | D                
  | MD                
  | A                  
  | AM                 
  | AD                
  | AMD               

type comp =
  | Zero
  | One
  | NegOne
  | DReg
  | AReg
  | MReg
  | NotD
  | NotA
  | NotM
  | NegD
  | NegA
  | NegM
  | DPlus1
  | APlus1
  | MPlus1
  | DMinus1
  | AMinus1
  | MMinus1
  | DPlusA
  | DPlusM
  | DMinusA
  | DMinusM
  | AMinusD
  | MMinusD
  | DAndA
  | DAndM
  | DOrA
  | DOrM

type jump =
  | NullJump       
  | JGT                
  | JEQ          
  | JGE               
  | JLT             
  | JNE               
  | JLE                
  | JMP              

type c_instruction = {
  dest : dest;
  comp : comp;
  jump : jump;
}

(* The overall instruction type for the Hack Assembly language *)
type instruction =
  | AInstruction of a_instruction
  | CInstruction of c_instruction
  | LInstruction of string (*Define the type of labels*)

