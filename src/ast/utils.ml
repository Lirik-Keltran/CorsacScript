open AST

let sum_number n1 n2 =
  match (n1, n2) with
  | Float f1, Float f2 -> Float (f1 +. f2)
  | Float f1, Int f2 -> Float (f1 +. float_of_int f2)
  | Int f1, Float f2 -> Float (float_of_int f1 +. f2)
  | Int f1, Int f2 -> Int (f1 + f2)

let sub_number n1 n2 =
  match (n1, n2) with
  | Float f1, Float f2 -> Float (f1 -. f2)
  | Float f1, Int f2 -> Float (f1 -. float_of_int f2)
  | Int f1, Float f2 -> Float (float_of_int f1 -. f2)
  | Int f1, Int f2 -> Int (f1 - f2)

let mul_number n1 n2 =
  match (n1, n2) with
  | Float f1, Float f2 -> Float (f1 *. f2)
  | Float f1, Int f2 -> Float (f1 *. float_of_int f2)
  | Int f1, Float f2 -> Float (float_of_int f1 *. f2)
  | Int f1, Int f2 -> Int (f1 * f2)

let div_number n1 n2 =
  match (n1, n2) with
  | Float f1, Float f2 -> Float (f1 /. f2)
  | Float f1, Int f2 -> Float (f1 /. float_of_int f2)
  | Int f1, Float f2 -> Float (float_of_int f1 /. f2)
  | Int f1, Int f2 -> Int (f1 / f2)

let equal_numbers n1 n2 =
  match (n1, n2) with
  | Float f1, Float f2 -> f1 = f2
  | Float f1, Int f2 -> f1 = float_of_int f2
  | Int f1, Float f2 -> float_of_int f1 = f2
  | Int f1, Int f2 -> f1 = f2

let string_of_number n =
  match n with Float f -> string_of_float f | Int i -> string_of_int i

let rec print_expr = function
  | Id id -> id
  | Func f ->
      "|fun|{ arg_name: " ^ print_expr f.arg_f ^ "; body" ^ print_expr f.body
      ^ " }"
  | FuncCall fc ->
      "|call|{ caller: " ^ print_expr fc.caller ^ "; arg: " ^ print_expr fc.arg
      ^ " }"
  | BinOp (e1, _, e2) ->
      "|binop|{ " ^ print_expr e1 ^ " OP " ^ print_expr e2 ^ " }"
  | Var v -> "|var|{ name: " ^ v.name ^ "; value: " ^ print_expr v.value ^ " }"
  | Number (Float n) -> "|float|" ^ string_of_float n
  | Number (Int n) -> "|int|" ^ string_of_int n
  | FuncOcaml _ -> "|system|{ system }"
  | Atom a -> "|atom|{ Atom: " ^ a ^ " }"
  | Tuple t ->
      " |tuple| {"
      ^ Array.fold_left (fun acc e -> acc ^ " " ^ print_expr e) "" t
      ^ " }"

let rec print_val = function
  | Id id -> id
  | Func _ -> "Func"
  | FuncCall _ -> "Call expr"
  | BinOp _ -> "Bin operation"
  | Number n -> string_of_number n
  | FuncOcaml _ -> "Func"
  | Atom a -> "{" ^ a ^ "}"
  | Var _ -> failwith "impossible token"
  | Tuple t ->
      "(" ^ Array.fold_left (fun acc e -> acc ^ " " ^ print_val e) "" t ^ " )"
