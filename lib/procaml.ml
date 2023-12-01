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
    | _ -> e

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

let rec perform_steps rules expression
 = match perform_step rules expression with
  | Some (nm, e) -> (nm, e) :: perform_steps rules e
  | None -> []

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

let prove rules lhs rhs
  = let lhs_steps = perform_steps rules lhs in
    let rhs_steps = perform_steps rules rhs in
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

let rec mkConstructorApp expr variantvars = match variantvars with
  | [] -> expr
  | [var, _] -> Application (expr, Identifier var)
  | (var, _)::rest ->
    Application (expr, mkConstructorApp (Application (Identifier ",", Identifier var)) rest)

let caseproof vars varnm typenm rules lhs rhs (variantnm, _variantvars) =
  let _ = List.map (fun x -> x) vars in
  print_endline typenm;
  let variant_namedvars = [("?1","int");("?2","list")] (* TODO: construct this from variantvars *) in
    let variant_expr = mkConstructorApp (Identifier variantnm) variant_namedvars in
    let caserule = ([], "case", Identifier varnm, variant_expr) in
  let ihs = [] (* TODO: generate the inductive hypotheses for all variant_namedvars that are of type typenm *) in
    let variantrules = caserule::ihs @ rules in
    ("Case "^ variantnm ^ ":") ::
    List.map (fun (vars, nm, lhs, rhs) -> nm^": "^String.concat "" (List.map (fun x -> "Forall "^x^". ") vars)^String_of.string_of_expression lhs^" = "^String_of.string_of_expression rhs) ihs @
    prove variantrules lhs rhs @ ["This completes the proof of case "^ variantnm ^ ".";""]

let inductionproof proof_name varnm _types vars rules lhs rhs =
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
  List.(concat (map (caseproof vars varnm typenm rules lhs rhs) variants)) @
  ["This completes the proof by induction."]
  
let rec prover types rules declarations =
      match declarations with
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), None) :: rest
            -> (* no hint, so let's prove this *)
               prove rules lhs rhs :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
         | ProofDeclaration (nm, vars, Equality (lhs,rhs), Some hint) :: rest
            -> (* we got a hint so we simply assume the statement *)
              (match hint with 
              | Axiom -> prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest
              | Induction x -> inductionproof nm x types vars rules lhs rhs :: prover types ((List.map typedVariableVariable vars,nm,lhs,rhs)::rules) rest)
        | TypeDeclaration (nm, variants) :: rest -> (* add the type to the list of types and keep going *)
          prover ((nm, variants)::types) rules rest
        | FunctionDeclaration _ :: rest -> prover types rules rest
        | [] -> []

let prover_main decls =
  prover [] [] decls |>
  List.map (String.concat "\n") |>
  String.concat "\n\n" |>
  print_endline