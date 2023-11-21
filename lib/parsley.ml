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

let string_of_parameter (parameter : parameter) =
  match parameter with
  | Parameter ((var : string), (varType : string)) -> "(" ^ var ^ " : " ^ varType ^ ")"

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
    | Match ((var : string), (matches : pattern list)) -> "match " ^ var ^ " with " ^ (
      let rec helper (patterns : pattern list) = 
      match patterns with
      | [] -> ""
      | h::tl -> string_of_pattern h ^ helper tl
      in helper matches
    )
and string_of_pattern (pattern : pattern) =
  match pattern with
  | Constructor ((name : string), (Some (parameters) : (string list) option)) -> begin
    "| " ^ name ^ " of " ^ (String.concat " * " parameters) 
  end
  | Constructor ((name : string), None) -> "| " ^ name ^ " "
  | Matchee ((name : string), None, (expression : expression)) -> "| " ^ name ^ " -> " ^ string_of_expression expression ^ " "
  | Matchee ((name : string), (Some (parameters) : (parameter list) option), (expression : expression)) -> begin
    let params_string = List.map string_of_parameter parameters in
    "| " ^ name ^ " (" ^ (String.concat ", " params_string) ^ ") -> " ^ string_of_expression expression ^ " "
  end

let string_of_equality (equality : equality) = 
  match equality with
  | Equality ((left : expression), (right : expression)) -> "(" ^ string_of_expression left ^ " = " ^ string_of_expression right ^ ")"

let string_of_hint (hint : hint option) =
  match hint with
  | Some Axiom -> "(*hint: axiom *)"
  | Some Induction (var : string) -> "(*hint: induction " ^ var ^ " *)"
  | _ -> "(*no hint*)"

let string_of_declaration (declaration : declaration) =
  match declaration with
  | Prove ((funcName : string), (parameters : parameter list), (equality : equality), (hint : hint option)) -> begin
    let params_string = List.map string_of_parameter parameters in
    "let (*prove*) " ^ funcName ^ (String.concat " " params_string) ^ " = " ^ string_of_equality equality ^ " " ^ string_of_hint hint
  end
  | Definition ((funcName : string), (parameters : parameter list), (expression : expression), (defType : string)) -> begin
    let params_string = List.map string_of_parameter parameters in
    "let rec " ^ funcName ^ (String.concat " " params_string) ^ " : " ^ defType ^ " = " ^ string_of_expression expression
  end
  | Variant ((name : string), (patterns : pattern list)) -> begin
    "type " ^ name ^ " = " ^
    let rec helper (patterns : pattern list) = 
      match patterns with
      | [] -> ""
      | h::tl -> string_of_pattern h ^ helper tl
    in helper patterns
  end