open Utils
open AST

let rec compile = function
  | Var v -> compile_var v
  | Func f -> compile_func f
  | FuncCall fc -> compile_funccall fc
  | BinOp bop -> compile_binop bop
  | Id id -> compile_id id
  | Tuple t -> compile_tuple t
  | Number n -> string_of_number n
  | FuncOcaml _ -> failwith ""
  | Atom a -> compile_atom a
  | Unknown -> ""
  | If (cond, e1, e2) -> compile_if (cond, e1, e2)

and compile_var _ = failwith "TODO"
and compile_func _ = failwith "TODO"
and compile_funccall _ = failwith "TODO"
and compile_binop _ = failwith "TODO"
and compile_id _ = failwith "TODO"
and compile_tuple _ = failwith "TODO"
and compile_atom _ = failwith "TODO"
and compile_if _ = failwith "TODO"
