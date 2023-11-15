include Ast
module Parser = Parser
module Lexer = Lexer

let parse (s : string) : statement list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast

(* let rec string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> name
  | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  ... of course, this part will depend on what your datatypes look like. *)

let ( >>= ) (opt: 'a option) (f: ('a -> string)) : string =
  match opt with
  | Some x -> f x
  | None -> ""

let string_of_expression (expr : expression) =
  let rec string_of_expression_helper (expr : expression) =
    match expr with
      | Variable x -> x
      | Application ((func : expression), (var : expression)) -> string_of_expression_helper func ^ " " ^ string_of_expression_helper var 
      | Tuple (vars : expression list) -> begin
        let lstLen = List.length vars in
        "(" ^ let rec string_of_exprs (exprs : expression list) (count : int)= 
          match exprs with
          | [] -> ""
          | h::tl -> string_of_expression_helper h ^ (if count!=lstLen then ", " else "") ^ string_of_exprs tl (count+1)
        in string_of_exprs vars 1 ^ ")"
      end
      | Parameter ((var : string), (varType : string)) -> "(" ^ var ^ " : " ^ varType ^ ")"
      | Equality ((left : expression), (right : expression)) -> "(" ^ string_of_expression_helper left ^ " = " ^ string_of_expression_helper right ^ ")"
  in (string_of_expression_helper expr)

let string_of_statement (statement : statement) =
  match statement with
  | Statement ((prove : prove option), (left : expression), (right : expression), (hint : hint option)) -> begin
    "let " ^ (prove >>= (fun prove -> if prove=Prove then "(*prove*) " else "")) ^ string_of_expression left ^ " = " ^ string_of_expression right
    ^ " " ^ (hint >>= (fun hint -> match hint with | Axiom -> "(*hint: axiom *)" | Induction (var : string) -> "(*hint: induction " ^ var ^ " )"))
  end