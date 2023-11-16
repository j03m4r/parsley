%{
  open Ast
%}

%token <string> VAR
%token LPAREN RPAREN COMMA EOF 
%token LET EQUALS COLON PROVE AXIOM INDUCTION TYPE VERTBAR OF STAR

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
  | LET ; equality = equality { Definition (equality) }
  | TYPE ; name = VAR ; EQUALS ; VERTBAR ; constructors = separated_nonempty_list(VERTBAR, pattern) { Variant (name, constructors) }
pattern:
  | name = VAR ; { Constructor (Variable name, None) }
  | name = VAR ; OF ; LPAREN ; params = separated_nonempty_list(STAR, expression) ; RPAREN { Constructor (Variable name, Some params) }
equality:
  | LPAREN ; left = expression ; EQUALS ; right = expression RPAREN { Equality (left, right) }
  | left = expression ; EQUALS ; right = expression { Equality (left, right) }
expression:
  | func = expression ; param = parameter { Application (func, param) }
  | func = expression ; arg = VAR { Application (func, Variable arg) }
  | func = expression ; LPAREN ; args = separated_nonempty_list(COMMA, expression) ; RPAREN { Application (func, Tuple args) }
  | var = VAR { Variable var }
parameter:
  | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }