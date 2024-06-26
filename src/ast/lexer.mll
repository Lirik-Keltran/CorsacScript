{
  open Parser

  exception SyntaxError of string
}

let white = [' ' '\t']+
let new_line = '\n'
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+
let low_letter = ['a'-'z']
let up_letter = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']
let id = (low_letter) (letter | digit | '_')*
let atom = up_letter (letter | digit)+
let comment = "//"

rule read =
  parse
  | comment           { Lexing.new_line lexbuf; read_comment lexbuf }
  | new_line          { Lexing.new_line lexbuf; read lexbuf }
  | white             { read lexbuf }
  | "*"               { MUL }
  | "+"               { PLUS }
  | "-"               { MINUS }
  | "/"               { DIV }
  | "("               { LPAREN }
  | ")"               { RPAREN }
  | ":"               { COLON }
  | "?"               { IF }
  | "<>"              { DESTRUCT }
  | "|>"              { PIPE }
  | "$"               { COMPOSE }
  | ">"               { MORE }
  | "<"               { LESS }
  | "<="              { LESSEQ }
  | ">="              { MOREEQ }
  | "="               { EQ }
  | "=="              { COMPARE }
  | ";"               { SEMI }
  | "."               { DOT }
  | ","               { COMMA }
  | id as word        { IDENT word }
  | atom as a         { ATOM a }
  | float as num      { FLOAT(float_of_string num) }
  | int as num        { INT (int_of_string num) }
  | "_"               { UNKNOWN }
  | eof               { EOF }
  | _                 { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
and read_comment =
  parse
  | new_line          { Lexing.new_line lexbuf; read lexbuf }
  | _                 { read_comment lexbuf }
  | eof               { EOF }
