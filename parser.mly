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
%type <Ast.const> main

%%
main:
    expr               { $1 }
;
expr:
  | INT                { Int $1 }
  | TRUE               { True }
  | FALSE              { False }
  | TRUE OR expr       { True }
  | FALSE OR expr      { $3 }
  | TRUE AND expr      { $3 }
  | FALSE AND expr     { False }
  | LPAREN expr RPAREN { $2 }
  | INT PLUS INT       { Int ($1 + $3) }
  | INT MINUS INT      { Int ($1 - $3) }
  | INT MULT INT       { Int ($1 * $3) }
  | INT DIV INT        { Int ($1 / $3) }
;
