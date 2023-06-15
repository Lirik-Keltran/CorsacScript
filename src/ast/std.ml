open AST
open Utils

let print e env = 
  let _ = print_val e |> print_endline
  in
  (Number (Int 0), env)


let std = 
  Env.empty 
  |> Env.add "print" (FuncOcaml ("print", print))
