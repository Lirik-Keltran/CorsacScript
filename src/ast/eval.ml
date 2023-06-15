open AST
open Utils

let rec eval list_e env = List.fold_left (fun acc e -> snd (eval_expr (e, acc))) env list_e
and eval_expr (e, env) = match e with
  | Var v         -> eval_var (v, env)
  | Func f        -> eval_func (f, env)
  | FuncCall fc   -> (eval_funccall (fc, env) |> fst, env)
  | BinOp bop     -> eval_binop (bop, env)
  | Id id         -> eval_id (id, env)
  | Tuple t       -> eval_tuple (t, env)
  | Number _      -> (e, env)
  | FuncOcaml _   -> (e, env)
  | Atom _        -> (e, env)

and eval_tuple (t, env) =
  let t' = Array.map (fun e -> eval_expr (e, env) |> fst ) t in 
    (Tuple t', env)

and eval_id (id, env) = 
 let e =  try Env.find id env
   with Not_found -> failwith ("Not found var: " ^ id)
  in
    match e with
      | e         -> (e, env)


and eval_var (v, env) =
  let env' = Env.add v.name v.value env in
  let (e', new_env) = eval_expr (v.value, env') in
    (e', Env.add v.name e' new_env)


and eval_func (f, env) =
  let env' = env in
  let f' = { 
    f with 
      env = env'; 
    } in
    (Func f', env)


and eval_binop ((e1, op, e2), env) = 
  let eval_operand e = eval_expr (e, env) |> fst
  in
  let print l r = ": " ^ print_expr l ^ " | " ^ print_expr r in
  match (eval_operand e1, op, eval_operand e2) with
    | (Number n1, Sum, Number n2) -> (Number (sum_number n1 n2), env)
    | (Number n1, Sub, Number n2) -> (Number (sub_number n1 n2), env)
    | (Number n1, Mul, Number n2) -> (Number (mul_number n1 n2), env)
    | (Number n1, Div, Number n2) -> (Number (div_number n1 n2), env)
    | (l, _, r)                   -> failwith ("Math error" ^ print l r)


and eval_funccall (fc, env) = 
  let arg = eval_expr (fc.arg, env) |> fst in
  let get_func id = match Env.find_opt id env with
    | Some Func f           -> Func f
    | Some FuncOcaml (n, f) -> FuncOcaml (n, f)
    | Some _                -> failwith (id ^ " have a type other than function")
    | None                  -> failwith ("Not found function with name" ^ id)
  in
  let (e, env') = match fc.caller with
    | FuncCall fc'  -> eval_funccall (fc', env)
    | Id id         -> (get_func id, env)
    | _             -> failwith "never"
  in
  match e with
    | Func f            -> 
      let env_f = Env.add f.arg_name arg f.env |> merge_env env' in
       eval_expr (f.body, env_f)
    | FuncOcaml (_, f)  -> f arg env'
    | _                 -> failwith "extra argument is passed"

