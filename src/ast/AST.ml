module Env = Map.Make (String)

let merge_env env1 env2 = Env.union (fun _ _ e -> Some e) env1 env2

type number = Float of float | Int of int
type opType = Sum | Div | Sub | Mul | Comp

type expr =
  | Var of var
  | Func of func
  | FuncCall of funccall
  | BinOp of binOp
  | Id of string
  | Number of number
  | Atom of string
  | Tuple of expr array
  | If of expr * expr * expr
  | Unknown
  | FuncOcaml of (expr -> expr Env.t -> expr * expr Env.t)
  | Dest of expr * expr

and binOp = expr * opType * expr
and func = { arg_f : expr; body : expr; env : expr Env.t }
and funccall = { caller : expr; arg : expr }
and var = { name : expr; value : expr }
