%{
  open Ast
%}
%token <string> IDENT
%token COLON
%token LPAREN
%token RPAREN
%token PROVE
%token LET
%token EQUAL
%token EOF
%token HINT
%token COMMA
%token ENDCOMMENT /* there is no startcomment, as it's called "hint", and proper comments are ignored by the lexer */
%token AXIOM
%start main
%type <declaration list> main
%%

main:
| list(declaration) EOF { $1 }
declaration:
| LET ; PROVE ; lemma_name=IDENT ; args = list(argument) ; EQUAL ; eq = equality ; hint=option(hint)
   { ProofDeclaration (lemma_name, args, eq, hint) }
argument:
| nm = IDENT; COLON; t = IDENT { TypedVariable (nm, t) }
| LPAREN ; arg = argument; RPAREN { arg }
equality:
| LPAREN ; e = equality ; RPAREN { e }
| lhs = expression ; EQUAL ; rhs = expression { Equality (lhs, rhs) }
hint:
| HINT ; AXIOM ; ENDCOMMENT { Axiom }
expression:
| LPAREN ; e = expression_with_commas ; RPAREN { e }
| lhs = expression ; arg = IDENT { Application (lhs, Identifier arg) }
| lhs = expression ; LPAREN ; arg = expression_with_commas ; RPAREN
   { Application (lhs, arg) }
| nm = IDENT { Identifier nm }

// these aren't in the gettingstarted.ml syntax,
// but here's a suggestion to deal with these anyways.
// We're using that "," is not a valid identifier
// We're using it as an identifier that stands for the function (fun x y -> (x, y))
// This also means we're representing (x,y,z) and ((x,y),z) as the same thing.
expression_with_commas:
| e = expression { e }
| e1 = expression_with_commas ; COMMA ; e2 = expression
  { Application (Application (Identifier ",", e1), e2)}



// %token <string> VAR
// %token LPAREN RPAREN COMMA EOF COLON STAR ARROW
// %token LET EQUALS PROVE AXIOM INDUCTION TYPE VERTBAR OF MATCH WITH 

// %start main
// %type <declaration list> main

// %%

// main:
//   | declaration1 = declaration ; declaration2 = main { declaration1 :: declaration2 }
//   | declaration = declaration ; EOF { declaration :: [] }
// declaration:
//   | PROVE ; expression = expression ; EQUALS ; equality = equality ; AXIOM { Prove (expression, equality, Axiom) }
//   | PROVE ; expression = expression ; EQUALS ; equality = equality ; INDUCTION ; var = VAR { Prove (expression, equality, Induction var) }
//   | PROVE ; expression = expression ; EQUALS ; equality = equality ; { Prove (expression, equality, Nil) }
//   | LET ; pattern = pattern ; EQUALS ; expression = expression { Definition (pattern, expression) }
//   | TYPE ; name = VAR ; EQUALS ; VERTBAR ; constructors = separated_nonempty_list(VERTBAR, pattern) { Variant (name, constructors) }
// pattern:
//   | expression = expression ; COLON ; funcType = VAR { Function (expression, funcType) }
//   | name = VAR ; { Constructor (Variable name, None) }
//   | name = VAR ; OF ; LPAREN ; params = separated_nonempty_list(STAR, expression) ; RPAREN { Constructor (Variable name, Some params) }
// equality:
//   | LPAREN ; left = expression ; EQUALS ; right = expression RPAREN { Equality (left, right) }
// expression:
//   | MATCH ; matchVar = VAR ; WITH ; VERTBAR ; matches = separated_nonempty_list(VERTBAR, matchee) { Match (Variable matchVar, matches) }
//   | func = expression ; param = parameter { Application (func, param) }
//   | func = expression ; arg = VAR { Application (func, Variable arg) }
//   | func = expression ; LPAREN ; args = separated_nonempty_list(COMMA, expression) ; RPAREN { Application (func, Tuple args) }
//   | var = VAR { Variable var }
//   | LPAREN ; expression = expression ; RPAREN { expression }
// matchee:
//   | name = VAR ; ARROW ; expression = expression { Matchee (Variable name, None, expression) }
//   | name = VAR ; LPAREN ; params = separated_nonempty_list(COMMA, parameter) ; RPAREN ; ARROW ; expression = expression { Matchee (Variable name, Some (Tuple params), expression) }
// parameter:
//   | LPAREN ; var = VAR ; COLON ; varType = VAR ; RPAREN { Parameter (var, varType) }