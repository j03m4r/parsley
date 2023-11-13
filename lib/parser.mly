%{
  open Ast
%}

%token <string> VAR
%token LET PROVE EQUALS LPAREN RPAREN COMMA EOF COLON
%token <string> HINT
%token AXIOM

%start main
%type <statement list> main

%%

main:
  | statements = separated_list (EOF, statement) ; EOF { statements }

statement:
  | LET ; PROVE ; id = VAR ; LPAREN ; arg = VAR ; RPAREN ; EQUALS ; expr = expr ; hint = option(hint) { Prove (id, expr, hint) }

hint:
  | HINT ; COLON ; AXIOM { Axiom }

expr:
  | func = expr ; arg = VAR { Application (func, Variable arg) }
  | func = expr ; LPAREN ; args = separated_nonempty_list(COMMA, expr) ; RPAREN { Application (func, Tuple args)}
  | var = VAR { Variable var }