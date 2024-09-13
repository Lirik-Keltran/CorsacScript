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

let get_new_type_index () =
  let _ = type_index.contents <- type_index.contents + 1 in
   Int.to_string type_index.contents

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
  args = [| Constructor { name; args = [||] } |];
}

let unknown_type = Constructor {
  name = "Unknown";
  args = [||];
}

let create_union_type args = Constructor {
  name = "Union";
  args
}

let float_type =  Constructor { name = "Float"; args = [||]; }
let int_type =  Constructor { name = "Int"; args = [||]; }

let rec to_string_type = function
  | Constructor tt -> to_string_constructor tt
  | Var tt -> tt ^ "`"
and to_string_constructor { name; args } =
  let constructor_text = Array.fold_left (fun acc tt -> acc ^ ", " ^ to_string_type tt) ("[:" ^ name) args in
   constructor_text ^ "]"

let print_type_env env = Env.iter (fun key tt -> "\n" ^ key ^ ": " ^ to_string_type tt |> print_endline) env

let rec unify type1 type2 env =
  (* Переименовать
  let unify_var type_key right env = match Env.find_opt type_key env with
    | Some t -> unify t right env
    | None -> Env.add type_key  right env
  in*)
  match type1, type2 with
  | Constructor c1,
    Constructor c2 ->
    if c1.name <> c2.name || Array.length c1.args <> Array.length c2.args
      then failwith (c1.name ^ " -!- " ^ c2.name)
      else
        let list1 = Array.to_list c1.args in
        let list2 = Array.to_list c2.args in
          List.fold_left2 (fun acc arg1 arg2 -> unify arg1 arg2 acc) env list1 list2
  | Var tt, right -> Env.add tt right env
  | left, Var tt -> Env.add tt left env


let rec get_type list_e =
  List.fold_left (fun acc e -> get_type_expr (e, acc) |> snd) Env.empty list_e

and get_type_expr ((e: expr), env) = match e with
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
    let t1, _ = get_type_expr (e1, env) in
    let t2, _ = get_type_expr (e2, env) in
    get_type_dest ((t1, t2), env)

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

and get_type_dest ((t1, t2), env) = (t1, unify t1 t2 env)

and get_type_func ({ arg_f; body; _ }, env) =
  let (arg_type, env') = get_type_expr (arg_f, env) in
  let ret_type = get_type_expr (body, env') |> fst in
  let function_type = create_function_type [| arg_type; ret_type |] in
    (function_type, env)

and get_type_binop ((e1, _, e2), env) =
  let get_type_operand e = get_type_expr (e, env) |> fst in
  let e1_type = get_type_operand e1 in
  let e2_type = get_type_operand e2 in
    (e1_type, unify e1_type e2_type env)

and get_type_funccall ({ caller; arg }, env) =
  let type_caller = get_type_expr (caller, env) |> fst in
  let type_call_arg = get_type_expr (arg, env) |> fst in
  match type_caller with
    | Constructor { name = _; args = [| arg_type; ret_type |] } ->
      let env' = unify type_call_arg arg_type env in
        let _ = print_endline
        (
          "AAAA: " ^
          (to_string_type type_call_arg) ^
          " - " ^
          (to_string_type arg_type)
      ) in
    let _ = print_type_env env' in
      (ret_type, env')
    | Var tname ->
      let func_type_name = Var (get_new_type_index ()) in

      let func_type = create_function_type [| type_call_arg; func_type_name |] in
      let env' = Env.add tname func_type env in
        (func_type, env')
    | _ -> failwith "FuncCall Error"


and get_type_id (id, env) = match Env.find_opt id env with
  | Some tt -> (tt, env)
  | None ->
    let id' = id ^ (get_new_type_index ()) in
    let tt = Var id' in
      (tt, Env.add id tt env)

and get_type_tuple (t, env) =
  let get_tuple_types e = get_type_expr (e, env) |> fst in
  let tuple_type = Array.map get_tuple_types t |> create_tuple_type in
  (tuple_type, env)

and get_type_if ((_, e1, e2), env) =
  let e1_type = get_type_expr (e1, env) |> fst in
  let e2_type = get_type_expr (e2, env) |> fst in
    if e1_type = e2_type
      then (e1_type, env)
      else (create_union_type [| e1_type; e2_type |], env)




