%{
    open AST
%}

%token <int> INT 
%token <float> FLOAT

// {string}
%token <string> IDENT
%token <string> ATOM

// + - * / = ; . |
%token PLUS MINUS MUL DIV EQ  SEMI DOT COMMA OR

// ( ) 
%token LPAREN RPAREN

// { } 
// %token LBRACE RBRACE

// [ ]
// %token LBRACKET RBRACKET

%token EOF

// задаем приоритет операций
// Фиктивный токен, для того чтобы задавать приоритет операций
// %nonassoc BELOW_SHARP
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
    | binop                 { BinOp $1 }
    | funcdecl              { Func $1 }
    | vardecl               { Var $1 }
    | funccall_expr         { $1 }
    | simple_expr           { $1 }
;

simple_expr:
    | id                                    { Id $1 }
    | number                                { Number $1 }
    | ATOM                                  { Atom $1}
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
    | IDENT EQ expr { { name = $1; value = $3; } }
;

funcdecl:
    | simple_expr DOT expr { { arg_f = $1; body = $3; env = Env.empty; } }
;

binop:
    | expr op expr { ($1, $2, $3) }
;

%inline op:
    | PLUS  { Sum }
    | DIV   { Div }
    | MINUS { Sub }
    | MUL   { Mul }
;

id:
    | IDENT { $1 }
;

number: 
    | INT { Int $1 }
    | FLOAT { Float $1 }
;
