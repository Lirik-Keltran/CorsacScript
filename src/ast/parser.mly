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

%token COMPARE MORE LESS LESSEQ MOREEQ
// _
%token UNKNOWN

// |>
%token PIPE

// ( )
%token LPAREN RPAREN

// <>
%token DESTRUCT

// $
%token COMPOSE

// { }
// %token LBRACE RBRACE

// [ ]
// %token LBRACKET RBRACKET

%token EOF

// задаем приоритет операций
// Фиктивный токен, для того чтобы задавать приоритет операций
// %nonassoc BELOW_SHARP
%right COMPOSE
%left COMPARE MORE LESS LESSEQ MOREEQ
%left PIPE
%left PLUS MINUS
%left MUL DIV
// %nonassoc UMINUS

// функция начала парсинга
%start prog
%type <AST.expr list> prog
%%
prog: command* EOF { $1 }

// TODO: убрать из синтаксиса точку с запятой
command: expr SEMI { $1 }

expr:
    | vardecl               { $1 }
    | func_expr           { $1 }
;

func_expr:
    | pipe                  { $1 }
    | compose               { $1 }
    | funcdecl              { $1 }
    | funccall_expr         { $1 }
    | ifexpr                { $1 }
    | destruct              { $1 }
    | simple_expr           { $1 }
;

simple_expr:
    | binop                                 { $1 }
    | id                                    { $1 }
    | number                                { $1 }
    | ATOM                                  { Atom $1}
    | UNKNOWN                               { Unknown }
    | LPAREN expr RPAREN                    { $2 }
    | tuple                                 { $1 }
;

tuple:
    | LPAREN func_expr COMMA tuple_args RPAREN   { Tuple (Array.of_list ($2 :: $4)) }
;

tuple_args:
    | func_expr COMMA tuple_args   { ($1 :: $3) }
    | func_expr                    { ($1 :: []) }
;

funccall_expr:
    | funccall_id simple_expr { FuncCall { caller = $1; arg = $2; } }
;

funccall_id:
    | simple_expr               { $1 }
;

vardecl:
    | id EQ expr { Var { name = $1; value = $3; } }
;

funcdecl:
    | simple_expr DOT expr { Func { arg_f = $1; body = $3; env = Env.empty; } }
;

ifexpr:
    | func_expr IF expr COLON expr { If ($1, $3, $5) }
;

binop:
    | simple_expr op simple_expr { BinOp ($1, $2, $3) }
;

pipe:
    | func_expr PIPE func_expr { FuncCall { caller = $3; arg = $1; }}
;

destruct:
    | simple_expr DESTRUCT simple_expr { Dest ($1, $3) }
;

compose:
    | func_expr COMPOSE func_expr
        {
            Func {
                arg_f = Id "arg";
                body = FuncCall {
                    caller = $1;
                    arg = FuncCall {
                        caller = $3;
                        arg = Id "arg";
                    };
                };
                env = Env.empty;
            }
        }
;

%inline op:
    | PLUS      { Sum }
    | DIV       { Div }
    | MINUS     { Sub }
    | MUL       { Mul }
    | COMPARE   { Comp }
    | LESSEQ    { LessEq }
    | MOREEQ    { MoreEq }
    | MORE      { More }
    | LESS      { Less }
;

id:
    | IDENT { Id $1 }
;

number:
    | INT { Number (Int $1) }
    | FLOAT { Number  (Float $1) }
;
