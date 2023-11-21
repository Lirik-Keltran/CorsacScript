open AST
open Utils



type componen = {
  name: string;
  types: string Array.t;
}

let print e env =
  let _ = print_val e |> print_endline in
  (Atom "Ok", env)

let debug_print e env =
  let _ = print_expr e |> print_endline in
  (Atom "Ok", env)

let get_atom e = match e with
  | Atom a -> a
  | _ -> failwith "Expected Atom"


let declare_system    e env =
  let name = get_atom e in
    (FuncOcaml (fun e env -> (e, env)), env)

let declare_component _ _ = failwith "c"
let declare_entity    _ _ = failwith "e"

let declare e env =
  match get_atom e with
  | "System" -> (FuncOcaml declare_system, env)
  | "Component" -> (FuncOcaml declare_component, env)
  | "Entity" -> (FuncOcaml declare_entity, env)
  | some -> failwith ("Expected System | Component | Entity, get " ^ some)


let std =
  Env.empty
  |> Env.add "print" (FuncOcaml print)
  |> Env.add "debug_print" (FuncOcaml debug_print)
  |> Env.add "declare" (FuncOcaml declare)
