open Ast
open Ast.Typecheck
open Ast.Utils
open Ast.Eval
open Ast.Std
open Core
open Lexing

let verbose = ref false
let execute = ref false
let path_to_script = ref None

let info = "crs <script_path> [-verbose] [-execute]"

let speclist = [
  ("-verbose", Arg.Set verbose, " - Show all Debug Info");
  ("-execute", Arg.Set execute, " - Execute script");
  ("-v", Arg.Set verbose, " - Show all Debug Info");
  ("-e", Arg.Set execute, " - Execute script");
]

let anon_fun filename = path_to_script := Some filename

let (>>) f g x = g(f(x));;

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

let print_verbose ast =
  let _ = print_endline " ----- Types ------" in
  let _ = get_type ast |> print_type_env in
  let _ = print_endline " -----  Ast  ------" in
  let _ = print_env_expr ast |> print_endline in
  print_endline " ----- ! ------"

let _ =
  let () = Arg.parse speclist anon_fun info in
  let path_to_script = match path_to_script.contents with
    | Some path -> path
    | None ->
      Arg.usage speclist info;
      invalid_arg "<script_path> is required";
  in
  let get_text_buf = In_channel.create >> Lexing.from_channel in
  let text = In_channel.create path_to_script in
  let lexbuf = Lexing.from_channel text in
  let () = Lexing.set_filename (get_text_buf path_to_script) path_to_script in
  let ast = parse_with_error lexbuf in
  let _ = match verbose.contents with
    | true -> print_verbose ast
    | _ -> ()
  in
  match execute.contents with
    | true ->
      let _ = print_endline "-" in
      let _ = eval ast std in
        ()
    | _ -> ()
