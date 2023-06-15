open AST
open Utils

let print e env = 
  let _ = match e with 
  | Number n  -> string_of_number n |> print_endline
  | Atom a    -> print_endline ("{" ^ a ^ "}")
  | _         -> failwith "Todo" 
  in
  (Number (Int 0), env)


let std = 
  Env.empty 
  |> Env.add "print" (FuncOcaml ("print", print))