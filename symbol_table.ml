module Env = Map.Make (String)

(*define types*)
type expr = 
     | Const of int
     | Var of string
     | Plus of expr * expr 
     | Let of string * expr * expr

type statement = 
     | Assign of string * expr
     | Print of expr

type program = statement list

(*helper function*)
let oper op x y = 
    match x with
    | Some x ->( match y with 
                 | Some y -> Some (op x y)
	         | None -> None)
    | None -> None

(*function to evaluate expressions*)
let rec eval env = function
    | Const x -> Some x
    | Var x -> Env.find_opt x env
    | Plus(exp1,exp2) -> let eval1 = eval env exp1 in 
                         let eval2 = eval env exp2 in 
			 oper Int.add eval1 eval2

    | Let(var,value,body) -> 
           let eval_value = eval env value in
	   ( match eval_value with 
	       | Some num -> let n_env = Env.add var num env in 
	                     eval n_env body 
	       | None -> None )

(*check the statements*)
let ex_statement env = function 
       | Assign(var,exp) -> 
               let eval_exp = eval env exp in 
	       ( match eval_exp with 
	          | Some num -> Env.add var num env
		  | None -> failwith "Undefined Variable")
       | Print exp -> 
               let value = eval env exp in 
	       ( match value with 
	          | Some num -> let () = Printf.printf"%d\n" num in env
	   	  | None -> failwith "Undefined Variable" ) 

(*interpreter which takes list of abstract type program*)
let interpret = List.fold_left ex_statement (Env.of_list [])

(*Sample program*)
(*Syntax: 
   x=5
   y=x+3
   Print y
   Let temp=x+y
   z=temp
   Print z
   Print (Let t=5 in x+t)
*)

(*above program in the list of abstract syntax*)
let sample_program = [
    Assign ("x", Const 5);
    Assign ("y", Plus (Var "x", Const 3));
    Print (Var "y");
    Assign ("z", Let ("temp", Plus (Var "x", Var "y"), Var "temp"));
    Print (Var "z");
    Print ( Let("t",Const 5,Plus (Var "x",Var "t")));
]

let _ = interpret sample_program
