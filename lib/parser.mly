%{
  open Ast
%}

%token <string> VAR
%token LPAREN RPAREN COMMA EOF LET EQUALS COLON PROVE AXIOM INDUCTION

%start main
%type <declaration list> main

%%

main:
  | declaration1 = declaration ; declaration2 = main { declaration1 :: declaration2 }
  | declaration = declaration ; EOF { declaration :: [] }
declaration:
  | PROVE ; pattern = pattern ; EQUALS ; equality = equality ; AXIOM { Prove (pattern, equality, Axiom) }
  | PROVE ; pattern = pattern ; EQUALS ; equality = equality ; INDUCTION ; var = VAR { Prove (pattern, equality, Induction var) }
  | PROVE ; pattern = pattern ; EQUALS ; equality = equality ; { Prove (pattern, equality, Nil) }
  | LET ; equality = equality { Definition (equality) }
pattern:
  | expr = expression { Function (expr) }
equality:
  | LPAREN ; left = expression ; EQUALS ; right = expression RPAREN { Equality (left, right) }
expression:
  | func = expression ; param = parameter { Application (func, param) }
  | func = expression ; arg = VAR { Application (func, Variable arg) }
  | func = expression ; LPAREN ; args = separated_nonempty_list(COMMA, expression) ; RPAREN { Application (func, Tuple args) }
  | var = VAR { Variable var }
parameter:
  | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }