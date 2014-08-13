%token TRUE FALSE
%token <int> INT
%token PLUS MINUS MULT DIV AND OR
%token LPAREN RPAREN
%left OR         /* lowest precedence */
%left AND
%left PLUS MINUS
%left MULT DIV   /* highest precedence */
%start main      /* the entry point */
%type <int> main
%%
main:
    expr               { $1 }
;
expr:
  | INT                { $1 }

/*
  | TRUE               { true }
  | FALSE              { false }
  | expr OR expr       { $1 || $3 }
  | expr AND expr      { $1 && $3 }
*/

  | LPAREN expr RPAREN { $2 }
  | expr PLUS expr     { $1 + $3 }
  | expr MINUS expr    { $1 - $3 }
  | expr MULT expr     { $1 * $3 }
  | expr DIV expr      { $1 / $3 }
;
