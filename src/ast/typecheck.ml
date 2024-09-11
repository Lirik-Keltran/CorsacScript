open AST
open Utils

type constructor = {
  name: string;
  args: exprtype array;
}

and exprtype =
  | Constructor of constructor
  | Var of int

(*

  (corsactype -> corsactype env.t) ->

*)
let create_tuple_type args = Constructor {
  name = "Tuple";
  args;
}

let create_atom_type name = Constructor {
  name = "Atom";
  args = [| Constructor { name; args = [||] } |];
}

let unknown_type = Constructor {
  name = "Unknown";
  args = [||];
}

(* Удалить потом *)
let mut variables_counter = 0

let rec unify type1 type2 env =
  match type1, type2 with
  | Constructor c1,
    Constructor c2 ->
    if c1.name <> c2.name
      then failwith (c1.name ^ " -!- " ^ c2.name)
    else if Array.length c1.args <> Array.length c2.args
      then failwith ("Args size" ^ c1.name ^ " -!- " ^ c2.name)
      else Array.map2 (fun arg1 arg2 -> unify arg1 arg2 env |> fst) c1.args c2.args
  | _, _ -> failwith ""

let rec get_type_expr ((e: expr), env) = match e with
  | Var v -> get_type_var (v, env)
  | Func f -> get_type_func (f, env)
  | FuncCall fc -> (get_type_funccall (fc, env) |> fst, env)
  | BinOp bop -> get_type_binop (bop, env)
  | Id id -> get_type_id (id, env)
  | Tuple t -> get_type_tuple (t, env)
  | Number n ->
    let num_type = match n with
    | Float _ -> "float"
    | Int _ -> "int" in
    let num = Constructor { name = num_type; args = [||]; }  in
      (num, env)
  | FuncOcaml _ -> failwith ""
  | Atom n -> (create_atom_type n, env)
  | Unknown -> (unknown_type, env)
  | If (cond, e1, e2) -> get_type_if ((cond, e1, e2), env)
  | Dest (e1, e2) ->
    let t1, _ = get_type_expr (e1, env) in
    let t2, _ = get_type_expr (e2, env) in
    get_type_dest((t1, t2), env)

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
and get_type_dest (_, _) = failwith ""
and get_type_func (_, _) = failwith ""
and get_type_funcall (_, _) = failwith ""
and get_type_binop (_, _) = failwith ""
and get_type_funccall (_, _) = failwith ""
and get_type_id (_, _) = failwith ""
and get_type_tuple (t, env) =
  let get_tuple_types e = get_type_expr (e, env) |> fst in
  let tuple_type = Array.map get_tuple_types t |> create_tuple_type in
  (tuple_type, env)
and get_type_if (_, _) = failwith ""
