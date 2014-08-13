%{
open Ast
%}

%token TRUE FALSE
%token <int> INT
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
    expr               { $1 }
;
expr:
  | INT                { Const (Int $1) }
  | TRUE               { Const True }
  | FALSE              { Const False }
  | TRUE OR expr       { Const True }
  | FALSE OR expr      { $3 }
  | TRUE AND expr      { $3 }
  | FALSE AND expr     { Const False }
  | LPAREN expr RPAREN { $2 }
  | INT PLUS INT       { Const (Int ($1 + $3)) }
  | INT MINUS INT      { Const (Int ($1 - $3)) }
  | INT MULT INT       { Const (Int ($1 * $3)) }
  | INT DIV INT        { Const (Int ($1 / $3)) }
;
