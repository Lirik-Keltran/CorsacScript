open Ast
open Ast.Eval
open Core
open Lexing
(* open Vm *)
(* let _ =
  let text = Core.In_channel.read_all "./script.crs" in
  let lexbuf = Lexing.from_string text in
  let ast = Parser.prog Lexer.read lexbuf in
  eval ast Std.std *)

let get_arg = Array.get (Sys.get_argv ())

let print_position outx lexbuf =
  let pos = Lexing.lexeme_start_p lexbuf in
  fprintf outx "filename: %s, line: %d, column:%d" pos.pos_fname
    pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error lexbuf =
    try Parser.prog Lexer.read lexbuf with
    | Lexer.SyntaxError msg -> fprintf stderr "%a: %s\n" print_position lexbuf msg;
      exit (-1)
    | Parser.Error -> fprintf stderr "%a: syntax error\n" print_position lexbuf;
      exit (-1)

let _ =
  let filename = try get_arg 1 with
  | Invalid_argument msg -> print_endline msg; exit (-1)
  in
  let text = In_channel.create filename in
  let lexbuf = Lexing.from_channel text in
  let () = Lexing.set_filename lexbuf filename in
  let ast = parse_with_error lexbuf in
    eval ast Std.std
