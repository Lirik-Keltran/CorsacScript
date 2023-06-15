%{
    open AST
%}

%token <int> INT 
%token <float> FLOAT

// {string}
%token <string> IDENT
%token <string> ATOM

// + - * / = ; .
%token PLUS MINUS MUL DIV EQ  SEMI DOT

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


command: expr SEMI { $1 }

expr:
    | binop                 { BinOp $1 }
    | funcdecl              { Func $1 }
    | vardecl               { Var $1 }
    | funccall_expr         { $1 }
    | simple_expr           { $1 }
;

simple_expr:
    | id                    { Id $1 }
    | number                { Number $1 }
    | ATOM                  { Atom $1}
    | LPAREN expr RPAREN    { $2 }
;

funccall_expr:
    | funccall_expr simple_expr { FuncCall { caller = $1; arg = $2; } }
    | simple_expr               { $1 }
;

vardecl:
    | IDENT EQ expr { { name = $1; value = $3; } }
;

funcdecl:
    | IDENT DOT expr { { arg_name = $1; body = $3; env = Env.empty; } }
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
