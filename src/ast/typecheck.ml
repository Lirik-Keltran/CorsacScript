open AST
open Utils

type numbertype =
  | Value of number
  | UnknownValue

type corsactype =
  | TUnknown of expr
  | TTuple of corsactype Array.t
  | TAtom of string
  | TFunc of corsactype * corsactype
  | TNumber of numbertype
  | TAny
  | TError

(*

  (corsactype -> corsactype env.t) ->

*)

let rec get_type_env list_e env =
  List.fold_left (fun acc e -> get_type_expr (e, acc) |> snd) env list_e

and get_type ((t, env): corsactype * corsactype Env.t) =
  match t with
  | TUnknown e -> get_type_expr (e, env)
  | _ -> (t, env)

and get_type_expr (e, env) = match e with
  | Var v -> get_type_var (v, env)
  | Func f -> get_type_func (f, env)
  | FuncCall fc -> (get_type_funccall (fc, env) |> fst, env)
  | BinOp bop -> get_type_binop (bop, env)
  | Id id -> get_type_id (id, env)
  | Tuple t -> get_type_tuple (t, env)
  | Number n ->
    let num = TNumber (Value n) in
      (num, env)
  | FuncOcaml (name, _) -> get_type_id (name, env)
  | Atom n -> (TAtom n, env)
  | Unknown -> (TAny, env)
  | If (cond, e1, e2) -> get_type_if ((cond, e1, e2), env)
  | Dest (e1, e2) ->
    let t1, _ = get_type_expr (e1, env) in
    let t2, _ = get_type_expr (e2, env) in
    get_type_dest((t1, t2), env)

and get_type_var (v, env) =  match (v.name, v.value) with
  | Id id, value ->
      let value_type, _ = get_type_expr (value, env) in
      let env' = Env.add id value_type env in
      (value_type, env')
  | _ -> failwith "todo"


and get_type_dest (t, env) = match t with
  | TTuple t1, TTuple t2 ->
      if Array.length t1 = Array.length t2 then
        let res = unite_tuple t1 t2 in
        let isSimular = Array.fold_left (fun acc (t1, t2) -> acc && type_is_simular t1 t2) true res
        in
          if isSimular
            then (TAtom "True", tuple_to_env env res)
            else (TAtom "False", env)
      else
        (TAtom "False", env)
  | Id id, e | e, Id id ->
    let v, _ = get_type_id (id, env) in
    get_type_dest ((v, TUnknown e), env)
  | _, _ -> (TAtom "False", env)

and get_type_func (_, _) = failwith ""
and get_type_funcall (_, _) = failwith ""
and get_type_binop (_, _) = failwith ""
and get_type_funccall (_, _) = failwith ""
and get_type_id (_, _) = failwith ""
and get_type_tuple (_, _) = failwith ""
and get_type_if (_, _) = failwith ""
and expr_to_type e1 e2 env =
  match (e1, e2) with
    | Unknown, _ | _, Unknown -> TAny
    | e, Id id | Id id, e -> get_type_var ({ name = Id id; value = e }, env) |> snd
    | Number a_num, Number f_num when a_num #= f_num -> env
    | Atom a_atom, Atom f_atom ->
        if a_atom = f_atom then env
        else
          failwith ("Atoms error: " ^ a_atom ^ " and " ^ f_atom ^ " is not equal")
    | Tuple t1, Tuple t2 -> unite_tuple t2 t1 |> tuple_to_env env
    | e1, e2 -> failwith (print_expr e1 ^ " | " ^ print_expr e2)

and tuple_to_env env args =
  Array.fold_left (fun env' (f, a) -> expr_to_type a f env') env args

and type_is_simular ty1 ty2 =
  match (ty1, ty2) with
    | TTuple t1, TTuple t2 ->
      if Array.length t1 = Array.length t2
        then
          unite_tuple t1 t2
          |> Array.fold_left (fun acc (e1, e2) -> acc && expr_is_simular e1 e2) true
        else false
    | _, TAny | TAny, _ -> true
    | TAtom n1, TAtom n2 -> n1 = n2
    | TNumber UnknownValue, TNumber _ | TNumber _, TNumber UnknownValue -> true
    | _, _ -> ty1 = ty2

and compare_type ty1 ty2 = failwith ""






