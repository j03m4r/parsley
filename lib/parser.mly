%{
  open Ast
%}
%token <string> VAR
%token LPAREN
%token RPAREN
%token COMMA
%token EOF
%start main
%type <expression list> main
%%
main:
  | e = expr; EOF { [e] }
expr:
  | var = VAR { Variable var }
  | func = expr ; arg = VAR { Application (func, Variable arg) }
  | func = expr ; LPAREN ; args = separated_nonempty_list(COMMA, expr) ; RPAREN { Application (func, Tuple args)}