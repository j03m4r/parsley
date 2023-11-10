include Ast
module Parser = Parser
module Lexer = Lexer

let parse (s : string) : expression list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast

(* let rec string_of_pattern (p : pattern) : string =
  match p with
  | Constructor (name, []) -> name
  | Constructor (name, patterns) -> name ^ " (" ^ (String.concat ", " (List.map string_of_pattern patterns)) ^ ")"
  ... of course, this part will depend on what your datatypes look like. *)

let string_of_declaration _p = "p"