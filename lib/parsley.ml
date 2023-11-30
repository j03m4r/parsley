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
    | h :: tl -> h ^ ";" ^ helper tl
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

let rec same_base_func (expr1 : expression) (expr2 : expression) =
  match (expr1, expr2) with
  | (Variable x1, Variable x2) -> x1=x2
  | (Application (func1, _), Application (func2, _)) -> same_base_func func1 func2
  | _ -> false

let rec attempt_substitution (variable : string) (expr1 : expression) (expr2 : expression) = 
  (* print_endline ("variable: " ^ variable ^ " attempting rewrite " ^ string_of_expression expr2 ^ " with " ^ string_of_expression expr1); *)
  if expr1=expr2 then Some (Singleton (variable, Variable variable)) else
  match (expr1, expr2) with
  | (Variable x, expr2) -> if (x=variable) then Some (Singleton (x, expr2)) else None
  | (Application (func1, arg1), Application (func2, Application (func3, arg2))) -> if func1=func2 then attempt_substitution variable arg1 (Application (func3, arg2)) else (let attempt = attempt_substitution variable expr1 (Application (func3, arg2)) in if attempt=None then attempt_substitution variable func1 (Application (func3, arg2)) else attempt)
  | (Application (func1, Variable x1), Application (func2, Variable x2)) -> if func1=func2 then attempt_substitution variable (Variable x1) (Variable x2) else (let attempt = attempt_substitution variable expr1 func2 in if attempt=None then attempt_substitution variable func1 func2 else attempt)
  | _ -> None
  
let rec generate_substitutions (subst_lst : substitution list) (variables : string list) (lhs : expression) (expr : expression) = 
  match variables with
  | [] -> subst_lst
  | h::tl -> let subst = attempt_substitution h lhs expr in (
    match subst with
    | Some x -> generate_substitutions (x::subst_lst) tl lhs expr
    | None -> generate_substitutions subst_lst tl lhs expr
    )
let rec decompose_substs (substs : (substitution option) list) (acc : substitution list) =
  match substs with
  | [] -> acc
  | (Some subst)::tl -> decompose_substs tl (subst::acc)
  | None::tl -> decompose_substs tl acc
      
(* 
  Function to generate substitutions. There will be at most n substitutions where n is the number of variables in the variables list.
  IF there is a sub-expression in "applications" expression (expression attempting to rewrite) that when substituted in for the respective variable in the "constants" 
  expression equals the "applications" expression, then create a substitution that maps the respective variable to that sub-expression
*)
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
      else (let arg = substitute (Singleton (x, expr)) arg in 
      let func = substitute (Singleton (x, expr)) func in Application (func, arg))
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
  (* print_endline ("original: " ^ string_of_expression orig ^ " lhs: " ^ string_of_expression lhs ^ " cur: " ^ string_of_expression cur); *)
  match orig with
  | Variable _ -> cur
  | Application (func, Application (innerfunc, arg)) -> if (Application (func, Application (innerfunc, arg)) = lhs) then cur else Application (func, fix_subst (Application (innerfunc, arg)) lhs cur)
  | Application (func, Variable x) -> if (Application (func, Variable x) = lhs) then cur else Application (fix_subst func lhs cur, Variable x)
  | _ -> cur

(* 
  Given an equality generate a list of substitutions and apply these substitutions to the rhs of the equality. However, this is
  problematic because you might lose some of the outside of the original expr so fix the substitution by reapplying the everything
  in the original expression before where the substitution begins. 

  Ex.
  match_expr [h] (cf (inv (h))) (inv (cf (cf (inv (h)))))
  >>
  [subst: h -> h]
  >>
  let res = substitute (h -> h) inv (cf (h))
  >>
  res = inv (cf (h))       but, inv (cf (h)) != inv (cf (inv (cf (h)))), which is what we want out of this substitution
  >>
  lhs = substitute (h -> h) cf (inv (h))
  >>
  lhs = cf (inv (h))
  >> 
  res = fix_subst (inv (cf (cf (inv (h))))) (cf (inv (h))) (inv (cf (h)))
  >>
  res = inv (cf (inv (cf (h)))) which is what we want
*)
let attempt_rewrite (variables : string list) (Equality (lhs, rhs) : equality) (expr : expression) =
  (* print_endline (string_of_variables variables ^ " rewriting " ^ string_of_expression expr ^ " ::::: equality at hand: " ^ string_of_equality  (Equality (lhs, rhs)));  *)
  let res = match_expression variables lhs expr in
  let rec helper lst expr rhs lhs =
    match lst with
    | [] -> expr
    | h::tl -> let rhs = substitute h rhs in (print_endline ("rhs subst res: " ^ string_of_expression rhs); let lhs = substitute h lhs in (print_endline ("lhs subst res: " ^ string_of_expression lhs);let fixed = fix_subst expr lhs rhs in helper tl fixed rhs lhs))
  in helper res expr rhs lhs
  
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
  (* (print_endline ("\n\nstarting " ^ string_of_expression expr ^ " # of equalities: " ^ string_of_int (List.length equalities))); *)
  match tryEqualities equalities expr with
  | None -> []
  | Some (nm, expr) -> (nm, expr) :: performSteps equalities expr

let rec steps_to_string (steps : (string * expression) list) (rev : bool) =
  match steps with
  | [] -> []
  | ((nm : string), (expr : expression)) :: tl -> if rev then (string_of_expression expr ^ "\n= { " ^ nm ^ " )") :: steps_to_string tl rev else ("= { " ^ nm ^ " )\n" ^ string_of_expression expr) :: steps_to_string tl rev

let rec get_variables (parameters : parameter list) : string list =
  match parameters with
  | [] -> []
  | Parameter (var, _) :: tl -> var :: get_variables tl

let rec get_common_step lhs_steps rhs_steps =
  match lhs_steps with
  | [] -> None
  | (h : (string * expression)) :: tl -> (
    let rec contains rhs_steps step =
      match rhs_steps with
      | [] -> false
      | h :: tl -> if h=step then true else contains tl step
    in let is_common_step = contains rhs_steps h in
    if is_common_step then Some h else get_common_step tl rhs_steps
  )

let rec combine_steps_until_common steps (c_nm, expr1) =
  match steps with
  | [] -> []
  | ((nm, expr2) : (string * expression)) :: tl -> if nm!=c_nm then (nm,expr2)::(combine_steps_until_common tl (c_nm, expr1)) else []

let string_of_step ((nm, expr) : (string * expression)) =
  "STEPPPPP: ={ " ^ nm ^ " }\n" ^ string_of_expression expr

let rec print_steps (steps : (string*expression) list) =
  match steps with
  | [] -> ()
  | h :: tl -> print_endline (string_of_step h); print_steps tl

let consolidate_steps eqs lhs rhs =
  let lhs_steps = performSteps eqs lhs in
  let rhs_steps = performSteps eqs rhs in
  if List.length rhs_steps = 0 && List.length lhs_steps != 0 then string_of_expression lhs :: steps_to_string lhs_steps false else
  let common_step = get_common_step lhs_steps rhs_steps in
  match common_step with
  | None -> (string_of_expression lhs :: steps_to_string lhs_steps false @ ["= { ??? }"] @ steps_to_string rhs_steps true @ [string_of_expression rhs])
  | Some (nm, expr) -> (
    let lhs_steps = combine_steps_until_common lhs_steps (nm, expr) in
    let rhs_steps = combine_steps_until_common rhs_steps (nm, expr) in
    (string_of_expression lhs :: steps_to_string lhs_steps false @ steps_to_string [(nm,expr)] false @ ["= { " ^ nm ^ " }"] @ steps_to_string rhs_steps true @ [string_of_expression rhs])
  )

let rec proofs_of_simple eqs (lst : declaration list) =
  match lst with
  | [] -> []
  | Prove ((funcName : string), (parameters : parameter list), (Equality (lhs, rhs) : equality), (hint : hint option)) :: decls -> 
    let variables = get_variables parameters in
    (match hint with
    | None -> (("Proof of " ^ funcName ^ ": ") :: (consolidate_steps eqs lhs rhs)) :: (proofs_of_simple ((funcName, variables, lhs, rhs) :: eqs) decls)
    | _ -> proofs_of_simple ((funcName, variables, lhs, rhs) :: eqs) decls)
  | _ :: decls -> proofs_of_simple eqs decls

let produce_output_simple (lst : declaration list)
  = print_endline (String.concat "\n\n" (List.map (String.concat "\n")
  (proofs_of_simple [] lst)))