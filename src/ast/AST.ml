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


(*
func
arg_f

f2 . f1 = arg. f2(f1 arg);

{
  arg_f: Id arg,
  body: funcall { caller: f2, arg: funccall { caller: f1, arg: arg}  },
  env,
}

{ arg_f = Id "arg", body = { caller = Id f2, arg: { caller: Id f1, arg: Id "arg" }} }
*)
