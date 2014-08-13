{
open Parser
exception Eof
}
rule token = parse
  | [' ' '\t' '\n']   { token lexbuf } (* skip blanks *)
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | ['a'-'Z']+ as lxm { VAR lxm }
  | "True"            { TRUE }
  | "False"           { FALSE }
  | "->"              { ARROW }
  | "let"             { LET }
  | "fun"             { FUN }
  | "||"              { OR }
  | "&&"              { AND }
  | "in"              { IN }
  | '='               { EQUAL }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { MULT }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | eof               { EOF }
