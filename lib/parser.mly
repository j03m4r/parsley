%{
  open Ast
%}

%token <string> VAR
%token LPAREN RPAREN COMMA EOF LET EQUALS COLON PROVE AXIOM INDUCTION

%start main
%type <statement list> main

%%

main:
  | statement1 = statement ; statement2 = main { statement1 :: statement2 }
  | statement = statement ; EOF { statement :: [] }
statement:
  | LET ; PROVE ; func = expression ; EQUALS ; expression = expression ; AXIOM{ Statement (Some (Prove), func, expression, Some (Axiom)) }
  | LET ; PROVE ; func = expression ; EQUALS ; expression = expression ; INDUCTION ; var = VAR { Statement (Some (Prove), func, expression, Some (Induction var)) }
  | LET ; PROVE ; func = expression ; EQUALS ; expression = expression ; { Statement (Some (Prove), func, expression, None) }
  | LET ; func = expression ; EQUALS ; expression = expression { Statement (None, func, expression, None) }
expression:
  | func = expression ; param = parameter { Application (func, param) }
  | LPAREN ; left = expression ; EQUALS ; right = expression ; RPAREN { Equality (left, right) }
  | func = expression ; arg = VAR { Application (func, Variable arg) }
  | func = expression ; LPAREN ; args = separated_nonempty_list(COMMA, expression) ; RPAREN { Application (func, Tuple args) }
  | var = VAR { Variable var }
parameter:
  | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }