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
