include Ast
module Parser = Parser
module Lexer = Lexer

let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast

(* let rec string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> name
  | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  ... of course, this part will depend on what your datatypes look like. *)

let rec string_of_expression (expr : expression) =
  match expr with
    | Variable x -> x
    | Application ((func : expression), (var : expression)) -> string_of_expression func ^ " " ^ string_of_expression var 
    | Tuple (vars : expression list) -> begin
      let lstLen = List.length vars in
      "(" ^ let rec string_of_exprs (exprs : expression list) (count : int) = 
        match exprs with
        | [] -> ""
        | h::tl -> string_of_expression h ^ (if count!=lstLen then ", " else "") ^ string_of_exprs tl (count+1)
      in string_of_exprs vars 1 ^ ")"
    end
    | Parameter ((var : string), (varType : string)) -> "(" ^ var ^ " : " ^ varType ^ ")"
    | Match ((var : expression), (matches : pattern list)) -> "match " ^ string_of_expression var ^ " with " ^ (
      let rec helper (patterns : pattern list) = 
      match patterns with
      | [] -> ""
      | h::tl -> string_of_pattern h ^ helper tl
      in helper matches
    )
and string_of_pattern (pattern : pattern) =
  match pattern with
  | Constructor ((name : expression), (Some (parameters) : expression list option)) -> begin
    "| " ^ string_of_expression name ^ " of " ^ (
      let lstLen = List.length parameters in
      let rec string_of_params (parameters : expression list) (count : int) = 
        match parameters with
        | [] -> ""
        | h::tl -> string_of_expression h ^ (if count!=lstLen then " * " else "") ^ string_of_params tl (count+1)
      in string_of_params parameters 1)
  end
  | Constructor ((name : expression), None) -> "| " ^ string_of_expression name ^ " "
  | Function ((func : expression), (funcType : string)) -> "let rec " ^ string_of_expression func ^ " : " ^ funcType
  | Matchee ((name : expression), None, (expression : expression)) -> "| " ^ string_of_expression name ^ " -> " ^ string_of_expression expression ^ " "
  | Matchee ((name : expression), (Some (parameters) : expression option), (expression : expression)) -> "| " ^ string_of_expression name ^ " " ^ string_of_expression parameters ^ " -> " ^ string_of_expression expression ^ " "

let string_of_equality (equality : equality) = 
  match equality with
  | Equality ((left : expression), (right : expression)) -> "(" ^ string_of_expression left ^ " = " ^ string_of_expression right ^ ")"

let string_of_hint (hint : hint) =
  match hint with
  | Axiom -> "(*hint: axiom *)"
  | Induction (var : string) -> "(*hint: induction " ^ var ^ " *)"
  | _ -> ""

let string_of_declaration (declaration : declaration) =
  match declaration with
  | Prove ((expression : expression), (equality : equality), (hint : hint)) -> begin
    "let (*prove*) " ^ string_of_expression expression ^ " = " ^ string_of_equality equality ^ " " ^ string_of_hint hint
  end
  | Definition ((pattern : pattern), (expression : expression)) -> string_of_pattern pattern ^ " = " ^ string_of_expression expression
  | Variant ((name : string), (patterns : pattern list)) -> begin
    "type " ^ name ^ " = " ^
    let rec helper (patterns : pattern list) = 
      match patterns with
      | [] -> ""
      | h::tl -> string_of_pattern h ^ helper tl
    in helper patterns
  end