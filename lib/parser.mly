%{
  open Ast
%}

%token <string> VAR
%token LPAREN RPAREN COMMA EOF COLON STAR ARROW
%token LET EQUALS PROVE AXIOM INDUCTION TYPE VERTBAR OF MATCH WITH HINT ENDCOMMENT

%start main
%type <declaration list> main

%start expression_eof
%type <expression> expression_eof

%%
expression_eof:
| e = simpleexpression ; EOF {e}

main:
  | list(declaration) ; EOF { $1 }
declaration:
  | PROVE ; funcName = VAR ; parameters = list(parameter) ; EQUALS ; equality = equality ; hint=option(hint) { Prove (funcName, parameters, equality, hint) }
  | LET ; funcName = VAR ; parameters = list(parameter) ; COLON ; funcType = VAR ; EQUALS ; expression = expression { Definition (funcName, parameters, expression, funcType) }
  | TYPE ; name = VAR ; EQUALS ; constructors = list(pattern) { Variant (name, constructors) }
pattern:
  | VERTBAR ; name = VAR ; params = option(constructorparams) { Constructor (name, params) }
  | VERTBAR ; name = VAR ; LPAREN ; params = option(separated_nonempty_list(COMMA, parameter)) ; RPAREN ; ARROW ; expression = expression { Matchee (name, params, expression) }
  | VERTBAR ; name = VAR ; ARROW ; expression = expression { Matchee (name, None, expression) }
constructorparams:
  | OF ; LPAREN ; params = separated_nonempty_list(STAR, strings) ; RPAREN { params }
equality:
  | LPAREN ; equality = equality ; RPAREN { equality }
  | left = expression ; EQUALS ; right = expression { Equality (left, right) }
expression:
  | LPAREN ; e = expression_with_commas ; RPAREN { e }
  | func = expression ; arg = VAR { Application (func, Variable arg) }
  | func = expression ; LPAREN ; arg = expression_with_commas ; RPAREN { Application (func, arg) }
  | MATCH ; matchVar = VAR ; WITH ; matches = list(pattern) { Match (matchVar, matches) }
  | var = VAR { Variable var }
hint:
  | HINT ; AXIOM ; ENDCOMMENT { Axiom }
  | HINT ; INDUCTION ; var = VAR ; ENDCOMMENT { Induction var }
parameter:
  | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }
strings:
  | name = VAR { name }
expression_with_commas:
| e = expression { e }
| e1 = expression_with_commas ; COMMA ; e2 = expression
  { Application (Application (Variable ",", e1), e2)}


simpleexpression:
  | func = simpleexpression ; arg = VAR { Application (func, Variable arg) }
  | func = simpleexpression ; LPAREN ; arg = simpleexpression ; RPAREN { Application (func, arg) }
  | var = VAR { Variable var }