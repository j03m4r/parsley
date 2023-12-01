include Ast

module Parser = Parser
module Lexer = Lexer

let parse (s : string) : expression =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.expression_eof Lexer.token lexbuf in
     ast

let string_of_declaration = String_of.string_of_declaration
let string_of_expression = String_of.string_of_expression

let empty = []
let singleton x y = [(x,y)]

let rec find x s = match s with
  | [] -> failwith ("Not found (find was passed a list that does not contain "^ x ^" as a key)")
  | (k,v) :: rm
    -> if x = k then v else find x rm

let rec compatible s1 s2 = match s1 with
  | [] -> true
  | (k,v) :: rest ->
      (match List.assoc_opt k s2 with
        | Some v' -> v = v' && compatible rest s2
        | None -> compatible rest s2)
let merge s1 s2 = if compatible s1 s2 then Some (s1 @ s2) else None

let rec match_expression variables pattern expression =
  match pattern with
    | Identifier x -> if List.mem x variables then
        Some (singleton x expression)
        else
        (if pattern = expression then Some empty else None)
    | Application (p1, p2) -> (* x has to be an application too, otherwise return None *)
        (match expression with
            | Application (e1, e2) ->
                (* We recursively match the sub-expressions.
                    This part is much easier to write if e2 is an expression (and it is for this particular ast),
                    because it's so symmetrical *)
                (match match_expression variables p1 e1, match_expression variables p2 e2 with
                | Some s1, Some s2 -> merge s1 s2
                | _ -> None)
            | _ -> None)
    | _ -> None

let rec substitute variables s e = match e with
    | Identifier var -> if List.mem var variables
                        then find var s
                        else e
    | Application (e1, e2) -> Application (substitute variables s e1, substitute variables s e2)
    | Match (var, pe_lst) -> Match (substitute variables s var, List.map (fun ((p : pattern), (e : expression)) -> (p, substitute variables s e)) pe_lst)

let rec attempt_rewrite variables lhs rhs expression =
  match match_expression variables lhs expression with
    | Some s -> Some (substitute variables s rhs)
    | None -> (match expression with
        | Application (e1, e2) -> (match attempt_rewrite variables lhs rhs e2 with
            | None -> 
              (match attempt_rewrite variables lhs rhs e1 with
                | None -> None
                | Some e1' -> Some (Application (e1', e2)))
            | Some e2' -> Some (Application (e1, e2')))
        | _ -> None (* not succesful *)
        )

let rec perform_step rules expression = match rules with
  | (variables, nm, lhs, rhs) :: rest -> (match attempt_rewrite variables lhs rhs expression with
      | Some e -> Some (nm, e)
      | _ -> perform_step rest expression)
  | [] -> None

let rec mkConstructorApp expr (variantvars : pattern list) = match variantvars with
  | [] -> expr
  | [Variable (var, _)] -> Application (expr, Identifier var)
  | Variable (var, _)::rest ->
    Application (expr, mkConstructorApp (Application (Identifier ",", Identifier var)) rest)
  | _ -> expr

let get_match expr =
  let rec get_expr pe_list var =
    match pe_list with
    | (Constructor (nm, vars), e) :: tl -> if (mkConstructorApp (Identifier nm) vars)=var then e else get_expr tl var
    | _ -> Identifier "Never going to happen"
  in match expr with
  | Match (var, pe_list) -> get_expr pe_list var
  | _ -> Identifier "what"
 
let attempt_apply_func (funcDefs : (string list * string * expression * expression) list) expr =
  let res = match perform_step funcDefs expr with
  | Some (nm, e) -> (nm, e) :: ("apply match", get_match e) :: []
  | None -> []
  in res

let rec perform_steps rules expression (funcDefs : (string list * string * expression * expression) list)
 = match perform_step rules expression with
  | Some (nm, e) -> (nm, e) :: perform_steps rules e funcDefs
  | None -> (match (attempt_apply_func funcDefs expression) with 
    | [(nm1, e1);(nm2, e2)] -> (nm1, e1) :: (nm2, e2) :: perform_steps rules e2 funcDefs
    | _ -> [])

let print_step e nm = [String_of.string_of_expression e; " = { " ^ nm ^ " }"]
let rec print_steps_n e steps n =
      match (steps, n) with
      | (_, 0) -> []
      | ((nm, e') :: rest, _) ->
        print_step e nm @ print_steps_n e' rest (n-1)
      | _ -> failwith "print_steps_n: n is larger than the number of steps"

let rec lockstep lhs rhs lhs_steps rhs_steps =
  if lhs = rhs then [String_of.string_of_expression lhs]
  else
  match (lhs_steps, rhs_steps) with
  | ((nm1, e1) :: rest1, (nm2, e2) :: rest2) ->
    print_step lhs nm1 @ lockstep e1 e2 rest1 rest2 @ List.rev (print_step rhs nm2)
  | (_ , _) ->
    [String_of.string_of_expression lhs; " = { ??? }"
    ; String_of.string_of_expression rhs]
let rec take_away_steps e steps n =
  match (steps, n) with
  | (_, 0) -> (e, steps)
  | ((_ , e') :: rest, _) ->
    take_away_steps e' rest (n-1)
  | _ -> failwith "take_away_steps: n is larger than the number of steps"

let prove rules lhs rhs (funcDefs : (string list * string * expression * expression) list)
  = let lhs_steps = perform_steps rules lhs funcDefs in
    let rhs_steps = perform_steps rules rhs funcDefs in
    let lhs_n = List.length lhs_steps in
    let rhs_n = List.length rhs_steps in
    if lhs_n > rhs_n then
      (let (lhs',lhs_steps') = take_away_steps lhs lhs_steps (lhs_n - rhs_n) in
      print_steps_n lhs lhs_steps (lhs_n - rhs_n) @ 
      lockstep lhs' rhs lhs_steps' rhs_steps
      )
      else
      (let (rhs',rhs_steps') = take_away_steps rhs rhs_steps (rhs_n - lhs_n) in
      lockstep lhs rhs' lhs_steps rhs_steps' @
      List.rev (print_steps_n rhs rhs_steps (rhs_n - lhs_n))
      )

let rec print_typedVariables lst str =
  match lst with
  | [] -> print_endline str
  | h::tl -> print_typedVariables tl (String_of.string_of_typedvariable h ^ "\n")

let gen_ih variables lhs rhs expression =
  match attempt_rewrite variables lhs rhs expression with
  | Some e -> e
  | None -> failwith "Var not in ih expr"

let caseproof (vars : typedVariable list) varnm typenm rules lhs rhs (funcDefs : (string list * string * expression * expression) list) (variantnm, _variantvars)  =
  let define_variant_namedvars typenm =
    match typenm with
    | "list" -> (match variantnm with 
      | "Nil" -> []
      | _ -> [Variable ("h", "idk"); Variable ("t", "idk")])
    | _ -> []
  in
  let variant_namedvars = define_variant_namedvars typenm (* TODO: construct this from variantvars *) in
    let variant_expr = mkConstructorApp (Identifier variantnm) variant_namedvars in
    let caserule = ([], "case", Identifier varnm, variant_expr) in
  let ihs = [([], "IH", (gen_ih [] (Identifier varnm) (Identifier "t") lhs), (gen_ih (List.map typedVariableVariable vars) (Identifier varnm) (Identifier "t") rhs))] (* TODO: generate the inductive hypotheses for all variant_namedvars that are of type typenm *) in
    let variantrules = caserule::ihs @ rules in
    ("Case "^ variantnm ^ ":") ::
    List.map (fun (vars, nm, lhs, rhs) -> nm ^ ": " ^ String.concat "" (List.map (fun x -> "Forall "^x^". ") vars)^
    String_of.string_of_expression lhs^" = "^String_of.string_of_expression rhs) ihs @
    prove variantrules lhs rhs funcDefs @ ["This completes the proof of case "^ variantnm ^ ".";""]

let inductionproof proof_name varnm _types vars rules lhs rhs (funcDefs : (string list * string * expression * expression) list) =
  let rec findvar (lst: typedVariable list) = match lst with
  | [] -> failwith "variable not found"
  | (TypedVariable (x, y)) :: rest -> if x = varnm then y else findvar rest 
  in let typenm = findvar vars in
  let rec findtype (lst: (string * (typevariant list)) list) = match lst with
  | [] -> failwith "type not found"
  | (name, typevars) :: rest -> if name = typenm then typevars else findtype rest
  in let typedvariants = findtype _types in
  let rec formatvariants (lst: typevariant list) = match lst with
  | [] -> []
  | (Variant (name, typevars)) :: rest -> (name, typevars) :: formatvariants rest
  in let variants = formatvariants typedvariants in
  ("Proof " ^ proof_name ^ " by induction on (" ^ varnm ^ " : " ^ typenm^"):") ::
  List.(concat (map (caseproof vars varnm typenm rules lhs rhs funcDefs) variants)) @
  ["This completes the proof by induction."]

let rec create_funcDef vars expr =
  match vars with
  | [] -> Identifier "This will never happen"
  | h :: [] -> Application(expr, Identifier h)
  | h :: tl -> (Application (create_funcDef tl expr, Identifier h))
  
let rec prover types rules (funcDefs : (string list * string * expression * expression) list) declarations =
      match declarations with
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), None) :: rest
            -> (* no hint, so let's prove this *)
               (prove rules lhs rhs funcDefs) :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) funcDefs rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), Some hint) :: rest
            -> (* we got a hint so we simply assume the statement *)
              (match hint with 
              | Axiom -> prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) funcDefs rest
              | Induction x -> (inductionproof nm x types vars rules lhs rhs funcDefs) :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) funcDefs rest)
        | TypeDeclaration (nm, variants) :: rest -> (* add the type to the list of types and keep going *)
          prover ((nm, variants)::types) rules funcDefs rest
        | FunctionDeclaration (TypedVariable (nm, _), vars, def) :: rest -> let vars = List.map typedVariableVariable vars in let lhs = create_funcDef (List.rev vars) (Identifier nm) in prover types rules ((vars, ("def. of " ^ nm), lhs, def) :: funcDefs) rest
        | [] -> []

let prover_main decls =
  prover [] [] [] decls |>
  List.map (String.concat "\n") |>
  String.concat "\n\n" |>
  print_endline