
type parameter = Parameter of string * string

type expression =
  | Application of expression * expression
  | Tuple of expression list
  | Variable of string
  | Match of string * (pattern list)
and pattern = 
  | Constructor of string * ((string list) option)
  | Matchee of string * ((parameter list) option) * expression
and equality =
  | Equality of expression * expression
and hint =
  | Axiom
  | Induction of string
and declaration =
  | Prove of string * parameter list * equality * hint option
  | Definition of string * parameter list * expression * string
  | Variant of string * pattern list

(* 
 had to go to volleyball but next things to work on:
  1. Finish conceptualizing ast for variants
    a. think about refractoring function defs into a new pattern variant
  2. Apply the new ast to the parser and printer
  3. Test with sample.ml and debug
 *)

(* 
let prove append_nilnil = (append Nil (Nil) = (Nil))
->
  Statement (Variable "append_nilnil", Equality (Application (Application (Variable "append", Variable "Nil")), Variable "Nil"))
Equality (Function (Variable "append_nilnil"), 
Equality (Application (Application (Variable "append", Variable "Nil"), Variable "Nil"), Variable "Nil", None))

let (*prove*) append_cons (h : int) (t : list) (l : list)
 = (append (Cons (h, t)) l = Cons (h, append t l))
->
 Equality (Function (ParameterApp(ParameterApp(ParameterApp(Variable "append_cons", 
 Parameter ("h", "int")), Parameter ("h", "int")), Parameter ("h", "int"))), 
 Equality(Application(Application(Variable "append", Variable "Nil"), Variable "Nil"), Variable "Nil", None), None) 
*)