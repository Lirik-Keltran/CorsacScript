{
  open Parser
}

let white = [' ' '\t' '\n']+
let digit = ['0'-'9']
let int = '-'? digit+
let float = '-'? digit+ '.' digit+
let low_letter = ['a'-'z']
let up_letter = ['A'-'Z']
let letter = ['a'-'z' 'A'-'Z']
let id = (low_letter | '_') (letter | digit | '_')?+
let atom = up_letter (letter | digit)?+


rule read =
  parse
  | white             { read lexbuf }
  | "*"               { MUL }
  | "+"               { PLUS }
  | "-"               { MINUS }
  | "/"               { DIV }
  | "("               { LPAREN }
  | ")"               { RPAREN }
  | "|"               { OR }
  (* | "{"               { LBRACE }
  | "}"               { RBRACE }
  | "["               { LBRACKET }
  | "]"               { RBRACKET } *)
  | "="               { EQ }
  | ";"               { SEMI }
  | "."               { DOT }
  | ","               { COMMA }
  | id as word        { IDENT word }
  | atom as a         { ATOM a }
  | float as num      { FLOAT(float_of_string num) }
  | int as num        { INT (int_of_string num) }
  | eof               { EOF }