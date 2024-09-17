open AST
open Utils

type constructor = {
  name: string;
  args: exprtype array;
}

and exprtype =
  | Constructor of constructor
  | Var of string

(*

  (corsactype -> corsactype env.t) ->

*)
let type_index = ref 0

let create_tuple_type args = Constructor {
  name = "Tuple";
  args;
}

let create_function_type args = Constructor {
  name = "Function";
  args
}

let create_atom_type name = Constructor {
  name = "Atom";
  args = [| Constructor { name = "'" ^ name; args = [||] } |];
}

let unknown_type = Constructor {
  name = "Unknown";
  args = [||];
}

let create_union_type args = Constructor {
  name = "Union";
  args
}

let create_lambda_type args = Constructor {
  name = "Lambda";
  args = [| create_function_type args |];
}

let float_type =  Constructor { name = "Float"; args = [||]; }
let int_type =  Constructor { name = "Int"; args = [||]; }

let rec to_string_type = function
  | Constructor tt -> to_string_constructor tt
  | Var tt -> tt ^ "`"

and to_string_constructor { name; args } = match name, args with
  | "Function", [| arg_type; ret_type |] -> to_string_type arg_type ^ " -> " ^ to_string_type ret_type
  | "Lambda", [| tt |] -> "(" ^ to_string_type tt ^ ")"
  | "Union", args ->
    let union_str = Array.fold_left (fun acc tt -> acc ^ to_string_type tt ^ " | ") "" args in
      String.sub union_str 0 (String.length union_str - 2)
  | "Tuple", args ->
    let tuple_str = Array.fold_left (fun acc tt -> acc ^ to_string_type tt ^ ", ") "" args in
      "(" ^ (String.sub tuple_str 0 (String.length tuple_str - 2)) ^ ")"
  | "Atom", [| tt |] -> "" ^ to_string_type tt ^ ""
  | _ -> let constructor_text = Array.fold_left (fun acc tt -> acc ^ ", " ^ to_string_type tt) (name) args in
   constructor_text


let rec create_unique_type_name name env =
  let get_new_type_index () =
    let _ = type_index.contents <- type_index.contents + 1 in
    Int.to_string type_index.contents
  in
  let type_name = name ^ get_new_type_index () in
  match Env.find_opt type_name env with
  | Some _ -> create_unique_type_name name env
  | None -> type_name

let print_type_env env = Env.iter (fun key tt -> "\n" ^ key ^ ": " ^ to_string_type tt |> print_endline) env

let todo () = failwith "TODO"

let rec unify type1 type2 env =
  match type1, type2 with
  | Constructor { name = "Lambda"; args = [| tt1 |] }, tt2
  | tt1, Constructor { name = "Lambda"; args = [| tt2 |] } -> unify tt1 tt2 env
  | Constructor c1,
    Constructor c2 ->
    if c1.name <> c2.name || Array.length c1.args <> Array.length c2.args
      then failwith (to_string_type type1 ^ " -!- " ^ to_string_type type2)
      else
        let list1 = Array.to_list c1.args in
        let list2 = Array.to_list c2.args in
          List.fold_left2 (fun acc arg1 arg2 -> unify arg1 arg2 acc) env list1 list2
  | Var name, tt
  | tt, Var name ->
      Env.add name tt env


let rec solve_types env = Env.mapi (fun name tt -> solve_type name tt env |> fst) env
and solve_type id tt env =
  let _ = print_endline "------>" in
  let _ = print_endline "--env--" in
  let _ = print_type_env env in
  let _ = print_endline "--var--" in
  let _ = print_endline id in
  let _ = to_string_type tt |> print_endline in
  let _ = print_endline "<------" in
  match tt with
  | Var type_name ->
    let tt = match Env.find_opt type_name env with
      | Some tt ->
        let (tt, env) = solve_type type_name tt env in
        let env = Env.add type_name tt env in
          (tt, env)
      | None -> (tt, env)
    in
      tt
  | Constructor { name; args } ->
    let args = Array.map (fun tt -> solve_type id tt env |> fst) args in
    let tt = Constructor { name; args } in
    let env = Env.add id tt env  in
      (tt, env)


let rec get_type list_e =
  List.fold_left (fun acc e -> get_type_expr (e, acc) |> snd) Env.empty list_e

and get_type_expr ((e: expr), env) =
  match e with
  | Var v -> get_type_var (v, env)
  | Func f -> get_type_func (f, env)
  | FuncCall fc -> get_type_funccall (fc, env)
  | BinOp bop -> get_type_binop (bop, env)
  | Id id -> get_type_id (id, env)
  | Tuple t -> get_type_tuple (t, env)
  | Number n ->
    let num_type = match n with
    | Float _ -> float_type
    | Int _ -> int_type in
      (num_type, env)
  | FuncOcaml _ -> (Constructor { name = "Unknown F"; args = [||] }, env)
  | Atom n -> (create_atom_type n, env)
  | Unknown -> (unknown_type, env)
  | If (cond, e1, e2) -> get_type_if ((cond, e1, e2), env)
  | Dest (e1, e2) ->
    let t1, env' = get_type_expr (e1, env) in
    let t2, env' = get_type_expr (e2, env') in
    get_type_dest ((t1, t2), env')

and get_type_var (v, env) =
  match (v.name, v.value) with
  | _, Unknown -> failwith "Unknown cannot be used as a variable value"
  | Id id, value ->
      let e', _ = get_type_expr (value, env) in
      (e', Env.add id e' env)
  | Tuple t1, Tuple t2 ->
    let type_t1 = get_type_tuple (t1, env) |> fst in
    let _ = get_type_tuple (t2, env) |> fst in
      (type_t1, env)
  | e1, e2 -> failwith (print_expr e1 ^ " -- " ^ print_expr e2)

and get_type_dest ((t1, t2), env) =
  (t1, unify t1 t2 env)

and get_type_func ({ arg_f; body; _ }, env) =
  let (arg_type, env') = match arg_f with
    | Id arg_name ->
      let arg_unique_name = create_unique_type_name arg_name env in
      let arg_type = Var arg_unique_name in
        (arg_type, Env.add  arg_unique_name arg_type env)
    | _ ->
      get_type_expr (arg_f, env)
  in
  let (ret_type, env') = get_type_expr (body, env') in
  let arg_type = solve_type "" arg_type env' |> fst in
  let ret_type = solve_type "" ret_type env' |> fst in
  let function_type = create_function_type [| arg_type; ret_type |] in
    (function_type, env')

and get_type_binop ((e1, _, e2), env) =
  let (e1_type, env') = get_type_expr (e1, env) in
  let (e2_type, env') = get_type_expr (e2, env') in
    (e1_type, unify e1_type e2_type env')

and get_type_funccall ({ caller; arg }, env) =
  let (type_caller, env') = get_type_expr (caller, env) in
  let (type_call_arg, env') = get_type_expr (arg, env') in
  match type_caller with
    | Constructor { name = _; args = [| arg_type; ret_type |] } ->
      let env' = unify type_call_arg arg_type env' in
      let env' = solve_types env' in
      let ret_type = solve_type "" ret_type env' |> fst in
      (ret_type, env')
    | Var tname ->
      let func_ret_type = Var (create_unique_type_name "" env) in
      let func_type = create_lambda_type [| type_call_arg; func_ret_type |] in
      let env' = Env.add tname func_type env' in
      let env' = solve_types env' in
        (func_ret_type, env')
    | _ -> failwith "FuncCall Error"


and get_type_id (id, env) =
  match Env.find_opt id env with
  | Some tt -> (tt, env)
  | None ->
    let id' = create_unique_type_name id env in
    let tt = Var id' in
      (tt, Env.add id tt env)

and get_type_tuple (t, env) =
  let env = ref env in
  let get_type_for_tuple_elem e =
    let (tt, env') = get_type_expr (e, env.contents) in
    let _ = env.contents <- env' in
    tt
  in
  let tuple_type = Array.map get_type_for_tuple_elem t |> create_tuple_type in
  (tuple_type, env.contents)

and get_type_if ((condition, e1, e2), env) =
  let (_, env') = get_type_expr (condition, env) in
  let (e1_type, env') = get_type_expr (e1, env') in
  let (e2_type, env') = get_type_expr (e2, env') in
    if e1_type = e2_type
      then (e1_type, env')
      else (create_union_type [| e1_type; e2_type |], env')




