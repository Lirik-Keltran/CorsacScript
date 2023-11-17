open AST
open Utils

let rec eval list_e env =
  List.fold_left (fun acc e -> eval_expr (e, acc) |> snd) env list_e

and eval_expr (e, env) =
  match e with
  | Var v -> eval_var (v, env)
  | Func f -> eval_func (f, env)
  | FuncCall fc -> (eval_funccall (fc, env) |> fst, env)
  | BinOp bop -> eval_binop (bop, env)
  | Id id -> eval_id (id, env)
  | Tuple t -> eval_tuple (t, env)
  | Number _ | FuncOcaml _ | Atom _ | Unknown -> (e, env)
  | If (cond, e1, e2) -> eval_if ((cond, e1, e2), env)
  | Dest (e1, e2) -> eval_dest((e1, e2), env)

and eval_if ((cond, e1, e2), env) =
  let (cond', env') = eval_expr (cond, env) in
  let result = if compare_expr cond' a_true then e1 else e2 in
  eval_expr (result, env')

and eval_tuple (t, env) =
  let t' = Array.map (fun e -> eval_expr (e, env) |> fst) t in
  (Tuple t', env)

and eval_id (id, env) =
  let e =
    try Env.find id env with Not_found -> failwith ("Not found var: " ^ id)
  in
  match e with e -> (e, env)

and eval_var (v, env) =
  match (v.name, v.value) with
  | _, Unknown -> failwith "Unknown cannot be used as a variable value"
  | Id id, value ->
      let env' = Env.add id value env in
      let e', new_env = eval_expr (value, env') in
      (e', Env.add id e' new_env)
  | Tuple t1, Tuple t2 -> (
      match eval_tuple (t2, env) |> fst with
      | Tuple t2' ->
          let env' = unite_tuple t1 t2' |> tuple_to_env env in
          (Tuple t2', env')
      | _ -> failwith "todo")
  | e1, e2 -> failwith (print_expr e1 ^ " -- " ^ print_expr e2)

and eval_func (f, env) =
  let env' = env in
  let f' = { f with env = env' } in
  (Func f', env)
and eval_binop ((e1, op, e2), env) =
  let eval_operand e = eval_expr (e, env) |> fst in
  let print l r = ": " ^ print_expr l ^ " | " ^ print_expr r in
  match (eval_operand e1, op, eval_operand e2) with
  | Number n1, Sum, Number n2 -> (Number (sum_number n1 n2), env)
  | Number n1, Sub, Number n2 -> (Number (sub_number n1 n2), env)
  | Number n1, Mul, Number n2 -> (Number (mul_number n1 n2), env)
  | Number n1, Div, Number n2 -> (Number (div_number n1 n2), env)
  | Number n1, More, Number n2    -> ((n1 #> n2) |> bool_to_abool, env)
  | Number n1, MoreEq, Number n2  -> ((n1 #>= n2) |> bool_to_abool, env)
  | Number n1, Less, Number n2    -> ((n1 #< n2) |> bool_to_abool, env)
  | Number n1, LessEq, Number n2  -> ((n1 #<= n2) |> bool_to_abool, env)
  | e1, Comp, e2 ->
      let result = if compare_expr e1 e2 then a_true else a_false in
      (result, env)
  | l, _, r -> failwith ("Math error" ^ print l r)

and eval_funccall (fc, env) =
  let arg = eval_expr (fc.arg, env) |> fst in
  let get_func id =
    match Env.find_opt id env with
    | Some (Func f) -> Func f
    | Some (FuncOcaml f) -> FuncOcaml f
    | Some _ -> failwith (id ^ " have a type other than function")
    | None -> failwith ("Not found function with name" ^ id)
  in
  let e, env' =
    match fc.caller with
    | FuncCall fc' -> eval_funccall (fc', env)
    | Id id -> (get_func id, env)
    | Func _ -> (fc.caller, env)
    | _ -> failwith "never"
  in
  match e with
  | Func f -> call_func f arg env'
  | FuncOcaml f -> f arg env'
  | _ -> failwith "extra argument is passed"

and call_func f arg env =
  let env' =
    match (f.arg_f, arg) with
    | Id id, _ -> Env.add id arg f.env
    | Tuple func_tuple, Tuple arg_tuple
      when Array.length func_tuple = Array.length arg_tuple ->
        unite_tuple func_tuple arg_tuple |> tuple_to_env env
    | Atom a1, Atom a2 when a1 = a2 -> env
    | e1, e2 -> failwith ("call_func " ^ print_expr e1 ^ " - " ^ print_expr e2)
  in
  eval_expr (f.body, env' |> merge_env env)

and expr_to_env e1 e2 env =
  match (e1, e2) with
  | e, Id id | Id id, e -> eval_var ({ name = Id id; value = e }, env) |> snd
  | Number a_num, Number f_num when a_num #= f_num -> env
  | Atom a_atom, Atom f_atom ->
      if a_atom = f_atom then env
      else
        failwith ("Atoms error: " ^ a_atom ^ " and " ^ f_atom ^ " is not equal")
  | Tuple t1, Tuple t2 -> unite_tuple t2 t1 |> tuple_to_env env
  | e1, e2 -> failwith (print_expr e1 ^ " | " ^ print_expr e2)

and tuple_to_env env args =
  Array.fold_left (fun env' (f, a) -> expr_to_env a f env') env args

and eval_dest ((e1, e2), env) = match (e1, e2) with
  | Tuple t1, Tuple t2 ->
      if Array.length t1 = Array.length t2 then
        let res = unite_tuple t1 t2 in
        let isSimular = Array.fold_left (fun acc (t1, t2) -> acc && expr_is_simular t1 t2) true res
        in
          if isSimular
            then (a_true, tuple_to_env env res)
            else (a_false, env)
      else
        (a_false, env)
  | Id id, e | e, Id id ->
    let v = eval_id (id, env) |> fst in
    eval_dest ((v, e), env)
  | _, _ -> (a_false, env)

















