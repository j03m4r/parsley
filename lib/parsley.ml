include Ast
module Parser = Parser
module Lexer = Lexer

type substitution = 
  | Empty
  | Singleton of string * expression (* maybe Singleton of string * substitution *)
let merge (substitution1 : substitution) (substitution2 : substitution) = 
  match (substitution1, substitution2) with
  | (Empty, Singleton ((name : string), (expr : expression))) -> Some (Empty, Singleton (name, expr))
  | (Singleton ((name : string), (expr : expression)), Empty) -> Some (Singleton (name, expr), Empty)
  | (Singleton ((name1 : string), (expr1 : expression)), Singleton ((name2 : string), (expr2 : expression))) -> begin
    if name1=name2 then (if expr1=expr2 then Some (Singleton (name1, expr1), Empty) else None) else Some (Singleton (name1, expr1), Singleton (name2, expr2))
  end
  | (Empty, Empty) -> Some (Empty, Empty)

let find (target : string) (((substitution1 : substitution), (substitution2 : substitution)) : (substitution * substitution)) : expression option =
  match (substitution1, substitution2) with
  | (Singleton ((name : string), (expr : expression)), Empty)
  | (Empty, Singleton ((name : string), (expr : expression))) -> if name=target then Some expr else None
  | (Singleton ((name1 : string), (expr1 : expression)), Singleton ((name2 : string), (expr2 : expression))) -> begin
    if name1=target then Some expr1 else (if name2=target then Some expr2 else None)
  end
  | (Empty, Empty) -> None










let parse (s : string) : declaration list =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.main Lexer.token lexbuf in
     ast

let parse_expression (s : string) : expression =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expression_eof
  Lexer.token lexbuf in ast

let string_of_parameter (parameter : parameter) =
  match parameter with
  | Parameter ((var : string), (varType : string)) -> "(" ^ var ^ " : " ^ varType ^ ")"

let rec string_of_expression (expr : expression) =
  match expr with
    | Variable x -> x
    | Application ((func : expression), (var : expression)) -> begin
      match func with
      | Variable x -> if x="," then "(" ^ string_of_expression var ^ ")," else x ^ " (" ^ string_of_expression var ^ ")"
      | _ -> string_of_expression func ^ " (" ^ string_of_expression var ^ ")"
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
    "let (*prove*) " ^ funcName ^ " " ^ (String.concat " " params_string) ^ " = " ^ string_of_equality equality ^ " " ^ string_of_hint hint
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

let string_of_subst (substitution : substitution) =
  match substitution with
  | Empty -> "empty\n"
  | Singleton (k, (expr : expression)) -> k ^ " -> " ^ string_of_expression expr ^ "\n"

let print_subst (s : substitution list) = 
  if List.length s = 0 then print_endline "None" else
  let rec helper (substs : substitution list) =
    match substs with 
    | [] -> ""
    | (substitution : substitution) :: tl -> string_of_subst substitution ^ (helper tl)
  in let subst_string = helper s
  in print_endline subst_string

let string_of_variables (variables : string list) = 
  let rec helper variables =
    match variables with
    | [] -> "]"
    | h :: tl -> h ^ "; " ^ helper tl
  in "[" ^ helper variables

let rec string_of_equalities eqs =
  match eqs with
  | [] -> ""
  | (nm, variables, lhs, rhs) :: tl -> nm ^ " " ^ string_of_variables variables ^ " " ^ string_of_expression lhs ^ " = " ^ string_of_expression rhs ^ "\n" ^ string_of_equalities tl

(* PROVING BELOW *)
(* PROVING BELOW *)
(* PROVING BELOW *)
(* PROVING BELOW *)
(* PROVING BELOW *)
(* PROVING BELOW *)
(* PROVING BELOW *)
(* PROVING BELOW *)

let rec func_in (expr : expression) (targ : expression) = 
  (* print_endline ("Attempting to equate " ^ string_of_expression expr ^ " with " ^ string_of_expression targ); *)
  match expr with
  | Variable x -> (
    match targ with
    | Variable y -> x=y
    | Application _ -> func_in targ (Variable x)
    | _ -> false
    )
  | Application (expr, _) -> func_in expr targ
  | _ -> false

let rec attempt_substitution (variable : string) (expr1 : expression) (expr2 : expression) = 
  match (expr1, expr2) with
  | (Variable x, expr2) -> if (x=variable) then Some (Singleton (x, expr2)) else None
  | (Application (func1, arg1), Application (func2, arg2)) -> if func1=func2 then attempt_substitution variable arg1 arg2 else attempt_substitution variable expr1 arg2
  | _ -> None
  
let rec generate_substitutions (subst_lst : substitution list) (variables : string list) (constants : expression) (applications : expression) = 
  match variables with
  | [] -> subst_lst
  | h::tl -> let subst = attempt_substitution h constants applications in (
    match subst with
    | Some x -> generate_substitutions (x::subst_lst) tl constants applications
    | None -> generate_substitutions subst_lst tl constants applications
    )
let rec decompose_substs (substs : (substitution option) list) (acc : substitution list) =
  match substs with
  | [] -> acc
  | (Some subst)::tl -> decompose_substs tl (subst::acc)
  | None::tl -> decompose_substs tl acc
      
let match_expression (variables : string list) (constants : expression) (applications : expression) =
  (* let substs = generate_substitutions [] variables constants applications
  in
  print_subst substs; *)
  (* (print_endline ("matching " ^ string_of_expression constants ^ " with " ^ string_of_expression applications ^ "\n")); *)
  generate_substitutions [] variables constants applications

let rec substitute (subst : substitution) (rhs : expression) =
  (* print_endline ("sub... rhs: " ^ (string_of_expression rhs) ^ " subst: " ^ (string_of_subst subst)); *)
  match rhs with
  | Application (func, arg) -> begin
    match subst with
    | Empty -> rhs
    | Singleton (x, expr) -> (
      if ((Variable x) = arg) then Application (func, expr)
      else (let arg = substitute (Singleton (x, expr)) arg in Application (func, arg))
    )
  end
  | Variable x1 -> begin
    match subst with
    | Empty -> rhs
    | Singleton (x2, expr) -> (
      if (x1=x2) then expr
      else rhs
    )
  end
  | _ -> rhs

let rec fix_subst (orig : expression) (lhs : expression) (cur : expression) =
  match orig with
  | Variable _ -> cur
  | Application (func, arg) -> if (Application (func, arg) = lhs) then cur else Application (func, fix_subst arg lhs cur)
  | _ -> cur

let attempt_rewrite (variables : string list) (Equality (lhs, rhs) : equality) (expr : expression) =
  (* print_endline (string_of_variables variables ^ " rewriting " ^ string_of_expression expr ^ " ::::: equality at hand: " ^ string_of_equality  (Equality (lhs, rhs)));  *)
  let res = match_expression variables lhs expr in
  let rec helper lst expr =
    match lst with
    | [] -> expr
    | h::tl -> let res = substitute h rhs in (let lhs = substitute h lhs in (let thing = fix_subst expr lhs res in helper tl thing))
  in helper res expr
  
let rec tryEqualities eq_lst (expr : expression) =
  match eq_lst with
  | [] -> None
  | ((nm : string), (variables : string list), (lhs : expression), (rhs : expression))::tl -> begin
    let res = attempt_rewrite variables (Equality (lhs, rhs)) expr in (
      (* print_endline ("Attempting to equate " ^ string_of_expression res ^ " with " ^ string_of_expression expr); *)
      if res != expr then Some (nm, res) else tryEqualities tl expr
    )
  end

let rec performSteps equalities (expr : expression) =
  match tryEqualities equalities expr with
  | None -> []
  | Some (nm, expr) -> (nm, expr) :: performSteps equalities expr

let rec steps_to_string (steps : (string * expression) list) =
  match steps with
  | [] -> []
  | ((nm : string), (expr : expression)) :: tl -> ("= { " ^ nm ^ " )\n" ^ string_of_expression expr) :: steps_to_string tl

let rec get_variables (parameters : parameter list) : string list =
  match parameters with
  | [] -> []
  | Parameter (var, _) :: tl -> var :: get_variables tl
 
let rec proofs_of_simple eqs (lst : declaration list) =
  match lst with
  | [] -> []
  | Prove ((funcName : string), (parameters : parameter list), (Equality (lhs, rhs) : equality), (hint : hint option)) :: decls -> 
    let variables = get_variables parameters in
    (match hint with
    | None -> (("Proof of " ^ funcName ^ ": ") :: (string_of_expression lhs :: steps_to_string (performSteps eqs lhs))) :: (proofs_of_simple ((funcName, variables, lhs, rhs) :: eqs) decls)
    | _ -> proofs_of_simple ((funcName, variables, lhs, rhs) :: eqs) decls)
  | _ :: decls -> proofs_of_simple eqs decls

let produce_output_simple (lst : declaration list)
  = print_endline (String.concat "\n\n" (List.map (String.concat "\n") 
  (proofs_of_simple [] lst)))