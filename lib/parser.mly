%{
  open Ast
%}
%token <string> VAR
%token <string> HINT
%token <string*string> TYPEDVAR
%token LPAREN
%token RPAREN
%token COMMA
%token LET
%token PARAMETER
%token EQUAL
%token EOF
%start main
%type <expression list> main
%%
main:
  | e = expr; EOF { [e] }
expr:
  | func = expr ; arg = VAR { Application (func, Variable arg) }
  | func = expr ; LPAREN ; args = separated_nonempty_list(COMMA, expr) ; RPAREN { Application (func, Tuple args)}
  | left = expr ; EQUAL ; right = expr ; hint = HINT
  | PARAMETER ; name = VAR ; varType = VAR { TypedVariable (name, varType) }
  | typedVar = TYPEDVAR { TypedVariable typedVar}
  | LET ; func = expr ; parameters = expr
  | var = VAR { Variable var }