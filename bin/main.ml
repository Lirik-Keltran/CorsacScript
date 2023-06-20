open Ast
open Ast.Eval

let _ =
  let text = Core.In_channel.read_all "./script.crs" in
  let lexbuf = Lexing.from_string text in
  let ast = Parser.prog Lexer.read lexbuf in
  eval ast Std.std
