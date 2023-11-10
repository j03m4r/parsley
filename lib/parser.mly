%{
  open Ast
%}
%token <string> VAR
%token LPAREN
%token RPAREN
%token COMMA
%token EOF
%start main
%type <expression> main
%%
main:
  | e = expr; EOF { e }
expr:
  | func = expr ; arg = VAR { Application (func, Variable arg) }
  | func = expr ; LPAREN ; args = separated_nonempty_list(COMMA, expr) ; RPAREN { 
    let rec rec_app lst =
      match lst with
      | [] -> Variable "null"
      | h::[] -> Application (func, h)
      | h::tl -> Application (h, rec_app tl)
    in rec_app args
    }
  | var = VAR; expr { Variable var }