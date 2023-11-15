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
      "(" ^ let rec string_of_exprs (exprs : expression list) (count : int)= 
        match exprs with
        | [] -> ""
        | h::tl -> string_of_expression h ^ (if count!=lstLen then ", " else "") ^ string_of_exprs tl (count+1)
      in string_of_exprs vars 1 ^ ")"
    end
    | Parameter ((var : string), (varType : string)) -> "(" ^ var ^ " : " ^ varType ^ ")"

let string_of_equality (equality : equality) = 
  match equality with
  | Equality ((left : expression), (right : expression)) -> "(" ^ string_of_expression left ^ " = " ^ string_of_expression right ^ ")"

let string_of_pattern (pattern : pattern) =
  match pattern with
  | Function (expression : expression) -> string_of_expression expression

let string_of_hint (hint : hint) =
  match hint with
  | Axiom -> "(*hint: axiom *)"
  | Induction (var : string) -> "(*hint: induction " ^ var ^ " *)"
  | _ -> ""

let string_of_declaration (declaration : declaration) =
  match declaration with
  | Prove ((pattern : pattern), (equality : equality), (hint : hint)) -> begin
    "let (*prove*) " ^ string_of_pattern pattern ^ " = " ^ string_of_equality equality ^ " " ^ string_of_hint hint
  end
  | Definition (equality : equality) -> string_of_equality equality