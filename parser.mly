%{
open Ast
%}

%token TRUE FALSE EOF
%token <int> INT
%token <string> VAR
%token PLUS MINUS MULT DIV AND OR
%token LPAREN RPAREN
%start main /* the entry point */
%type <Ast.expr> main

%%
main:
    expr3 EOF           { $1 }
;

expr3:
  | expr3 OR expr2      { Bin_op ($1, Or, $3) }
  | expr3 AND expr2     { Bin_op ($1, And, $3) }
  | expr2               { $1 }

expr2:
  | expr2 PLUS expr1    { Bin_op ($1, Plus, $3) }
  | expr2 MINUS expr1   { Bin_op ($1, Minus, $3) }
  | expr1               { $1 }

expr1:
  | expr1 MULT expr0    { Bin_op ($1, Mult, $3) }
  | expr1 DIV expr0     { Bin_op ($1, Div, $3) }
  | expr0               { $1 }

expr0:
  | INT                 { Const (Int $1) }
  | VAR                 { Var $1 }
  | TRUE                { Const True }
  | FALSE               { Const False }
  | LPAREN expr0 RPAREN { $2 }
;
