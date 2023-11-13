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

let rec string_of_declaration (p : expression) =
  match p with
    | Variable x -> x
    | Application ((func : expression), (var : expression)) -> string_of_declaration func ^ " " ^ string_of_declaration var 
    | Tuple (vars : expression list) -> begin
      let lstLen = List.length vars in
      "(" ^ let rec string_of_exprs (exprs : expression list) (count : int)= 
        match exprs with
        | [] -> ""
        | h::tl -> string_of_declaration h ^ (if count!=lstLen then ", " else "") ^ string_of_exprs tl (count+1)
      in string_of_exprs vars 1 ^ ")"
    end
  (* match p with
  | [] -> "\n"
  | (h : expression)::tl -> begin
    match h with
    | Variable x -> x
    | Application ((func : expression), (var : expression)) -> string_of_declaration [func] ^ " (" ^ string_of_declaration [var] ^ ")"
    | Tuple (vars : expression list) -> "(" ^ string_of_declaration vars ^ ")"
    ^ "\n" ^
    string_of_declaration tl
  end *)