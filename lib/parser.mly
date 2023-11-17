%{
  open Ast
%}

%token <string> VAR
%token LPAREN RPAREN COMMA EOF COLON STAR ARROW
%token LET EQUALS PROVE AXIOM INDUCTION TYPE VERTBAR OF MATCH WITH 

%start main
%type <declaration list> main

%%

main:
  | declaration1 = declaration ; declaration2 = main { declaration1 :: declaration2 }
  | declaration = declaration ; EOF { declaration :: [] }
declaration:
  | PROVE ; expression = expression ; EQUALS ; equality = equality ; AXIOM { Prove (expression, equality, Axiom) }
  | PROVE ; expression = expression ; EQUALS ; equality = equality ; INDUCTION ; var = VAR { Prove (expression, equality, Induction var) }
  | PROVE ; expression = expression ; EQUALS ; equality = equality ; { Prove (expression, equality, Nil) }
  | LET ; pattern = pattern ; EQUALS ; expression = expression { Definition (pattern, expression) }
  | TYPE ; name = VAR ; EQUALS ; VERTBAR ; constructors = separated_nonempty_list(VERTBAR, pattern) { Variant (name, constructors) }
pattern:
  | expression = expression ; COLON ; funcType = VAR { Function (expression, funcType) }
  | name = VAR ; { Constructor (Variable name, None) }
  | name = VAR ; OF ; LPAREN ; params = separated_nonempty_list(STAR, expression) ; RPAREN { Constructor (Variable name, Some params) }
equality:
  | LPAREN ; left = expression ; EQUALS ; right = expression RPAREN { Equality (left, right) }
expression:
  | MATCH ; matchVar = VAR ; WITH ; VERTBAR ; matches = separated_nonempty_list(VERTBAR, matchee) { Match (Variable matchVar, matches) }
  | func = expression ; param = parameter { Application (func, param) }
  | func = expression ; arg = VAR { Application (func, Variable arg) }
  | func = expression ; LPAREN ; args = separated_nonempty_list(COMMA, expression) ; RPAREN { Application (func, Tuple args) }
  | var = VAR { Variable var }
  | LPAREN ; var = VAR ; RPAREN { Variable var }
matchee:
  | name = VAR ; ARROW ; expression = expression { Matchee (Variable name, None, expression) }
  | name = VAR ; LPAREN ; params = separated_nonempty_list(COMMA, parameter) ; RPAREN ; ARROW ; expression = expression { Matchee (Variable name, Some (Tuple params), expression) }
parameter:
  | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }