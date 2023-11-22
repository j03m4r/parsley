%{
  open Ast
%}

%token <string> VAR
%token LPAREN RPAREN COMMA EOF COLON STAR ARROW
%token LET EQUALS PROVE AXIOM INDUCTION TYPE VERTBAR OF MATCH WITH HINT ENDCOMMENT

%start main
%type <declaration list> main

%%

main:
  | list(declaration) ; EOF { $1 }
declaration:
  | PROVE ; funcName = VAR ; parameters = list(parameter) ; EQUALS ; equality = equality ; hint=option(hint) { Prove (funcName, parameters, equality, hint) }
  | LET ; funcName = VAR ; parameters = list(parameter) ; COLON ; funcType = VAR ; EQUALS ; expression = expression { Definition (funcName, parameters, expression, funcType) }
  | TYPE ; name = VAR ; EQUALS ; constructors = list(pattern) { Variant (name, constructors) }
pattern:
  | VERTBAR ; name = VAR ; OF ; LPAREN ; params = separated_nonempty_list(STAR, strings) ; RPAREN { Constructor (name, Some params) }
  | VERTBAR ; name = VAR ; LPAREN ; params = option(separated_nonempty_list(COMMA, parameter)) ; RPAREN ; ARROW ; expression = expression { Matchee (name, params, expression) }
  | VERTBAR ; name = VAR ; ARROW ; expression = expression { Matchee (name, None, expression) }
  | VERTBAR ; name = VAR { Constructor (name, None) }
equality:
  | LPAREN ; left = expression ; EQUALS ; right = expression RPAREN { Equality (left, right) }
expression:
  | MATCH ; matchVar = VAR ; WITH ; matches = list(pattern) { Match (matchVar, matches) }
  | func = expression ; arg = VAR { Application (func, Variable arg) }
  | func = expression ; LPAREN ; args = separated_nonempty_list(COMMA, expression) ; RPAREN { Application (func, Tuple args) }
  | LPAREN ; var = VAR ; RPAREN { Variable var }
  | var = VAR { Variable var }
hint:
  | HINT ; AXIOM ; ENDCOMMENT { Axiom }
  | HINT ; INDUCTION ; var = VAR ; ENDCOMMENT { Induction var }
parameter:
  | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }
strings:
  | name = VAR { name }
