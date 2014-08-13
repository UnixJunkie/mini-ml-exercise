{
open Parser
exception Eof
}
rule token = parse
  | [' ' '\t' '\n']   { token lexbuf } (* skip blanks *)
  | ['0'-'9']+ as lxm { INT(int_of_string lxm) }
  | "True"            { TRUE }
  | "False"           { FALSE }
  | "||"              { OR }
  | "&&"              { AND }
  | '+'               { PLUS }
  | '-'               { MINUS }
  | '*'               { MULT }
  | '/'               { DIV }
  | '('               { LPAREN }
  | ')'               { RPAREN }
  | eof               { raise Eof }
