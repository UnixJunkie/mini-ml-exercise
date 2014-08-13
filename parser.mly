%{
open Ast
%}

%token TRUE FALSE EOF
%token <int> INT
%token <string> VAR
%token PLUS MINUS MULT DIV AND OR
%token LPAREN RPAREN
%left OR         /* lowest precedence */
%left AND
%left PLUS MINUS
%left MULT DIV   /* highest precedence */
%start main      /* the entry point */
%type <Ast.expr> main

%%
main:
    expr EOF              { $1 }
;

expr:
  | TRUE OR expr       { Const True }
  | FALSE OR expr      { $3 }
  | TRUE AND expr      { $3 }
  | FALSE AND expr     { Const False }
  | INT PLUS INT       { Const (Int ($1 + $3)) }
  | INT MINUS INT      { Const (Int ($1 - $3)) }
  | expr1              { $1 }

expr1:
  | expr1 MULT expr0   { Bin_op ($1, Mult, $3) }
  | expr1 DIV expr0    { Bin_op ($1, Div, $3) }
  | expr0              { $1 }

expr0:
  | INT                { Const (Int $1) }
  | VAR                { Var $1 }
  | TRUE               { Const True }
  | FALSE              { Const False }
  | LPAREN expr RPAREN { $2 }
;
