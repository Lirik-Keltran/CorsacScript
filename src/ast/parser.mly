%{
    open AST
%}

%token <int> INT
%token <float> FLOAT

// {string}
%token <string> IDENT
%token <string> ATOM

// +
%token PLUS
// -
%token MINUS
// *
%token MUL
// /
%token DIV
// =
%token EQ
// ;
%token SEMI
// .
%token DOT
// ,
%token COMMA
// ?
%token IF
// :
%token COLON
// ==
%token COMPARE
// _
%token UNKNOWN

// |>
%token PIPE

// ( )
%token LPAREN RPAREN

// <>
%token DESTRUCT

// { }
// %token LBRACE RBRACE

// [ ]
// %token LBRACKET RBRACKET



%token EOF

// задаем приоритет операций
// Фиктивный токен, для того чтобы задавать приоритет операций
// %nonassoc BELOW_SHARP
%left COMPARE PIPE
%left PLUS MINUS
%left MUL DIV DESTRUCT
// %nonassoc UMINUS

// функция начала парсинга
%start prog
%type <AST.expr list> prog
%%
prog: command* EOF { $1 }

// TODO: убрать из синтаксиса точку с запятой
command: expr SEMI { $1 }

expr:
    | funcdecl              { $1 }
    | vardecl               { Var $1 }
    | funccall_expr         { $1 }
    | ifexpr                { $1 }
    | simple_expr           { $1 }
;

simple_expr:
    | id                                    { $1 }
    | number                                { $1 }
    | ATOM                                  { Atom $1}
    | UNKNOWN                               { Unknown }
    | pipe                                  { $1 }
    | destruct                              { $1 }
    | binop                                 { $1 }
    | LPAREN expr RPAREN                    { $2 }
    | LPAREN expr COMMA tuple_args RPAREN   { Tuple (Array.of_list ($2 :: $4)) }
;

tuple_args:
    | expr COMMA tuple_args   { ($1 :: $3) }
    | expr                    { ($1 :: []) }
;

funccall_expr:
    | funccall_expr simple_expr { FuncCall { caller = $1; arg = $2; } }
    | simple_expr               { $1 }
;

vardecl:
    | simple_expr EQ expr { { name = $1; value = $3; } }
;

funcdecl:
    | simple_expr DOT expr { Func { arg_f = $1; body = $3; env = Env.empty; } }
;

ifexpr:
    | simple_expr IF expr COLON expr { If ($1, $3, $5) }
;

binop:
    | simple_expr op simple_expr { BinOp ($1, $2, $3) }
;

pipe:
    | simple_expr PIPE simple_expr { FuncCall { caller = $3; arg = $1; }}
;

destruct:
    | simple_expr DESTRUCT simple_expr { Dest ($1, $3) }
;

%inline op:
    | PLUS      { Sum }
    | DIV       { Div }
    | MINUS     { Sub }
    | MUL       { Mul }
    | COMPARE   { Comp }
;

id:
    | IDENT { Id $1 }
;

number:
    | INT { Number (Int $1) }
    | FLOAT { Number  (Float $1) }
;
