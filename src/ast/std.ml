open AST
open Utils

let print e env =
  let _ = print_val e |> print_endline in
  (Atom "Ok", env)

let debug_print e env =
  let _ = print_expr e |> print_endline in
  (Atom "Ok", env)

let std =
  Env.empty
  |> Env.add "print" (FuncOcaml print)
  |> Env.add "debug_print" (FuncOcaml debug_print)
