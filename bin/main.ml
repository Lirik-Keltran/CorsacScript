(* open Ast *)
(* open Ast.Eval *)
open Vm

(* let _ =
  let text = Core.In_channel.read_all "./script.crs" in
  let lexbuf = Lexing.from_string text in
  let ast = Parser.prog Lexer.read lexbuf in
  eval ast Std.std *)

let _ = let _ = virt |> run_vm in
  print_vm_heap virt
