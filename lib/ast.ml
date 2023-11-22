type typevariant = Variant of (string * string list)
type typedVariable = TypedVariable of (string * string)

(* TODO: Add match statements *)
type expression
= Application of (expression * expression)
| Variable of string
| Match of expression * (pattern list)
type pattern
= Constructor of (expression * (expression list) option)
| Function of (string * string)
| Matchee of (expression * expression option * expression)
type equality
= Equality of (expression * expression)
type hint
= Axiom
| Induction of string

type declaration
= ProofDeclaration of (string * typedVariable list * equality * hint option)
| DefinitionDeclaration of (pattern * expression)


(* type expression =
  | Application of expression * expression
  | Tuple of expression list
  | Variable of string
  | Parameter of string * string
  | Match of expression * (pattern list)
and pattern = 
  | Constructor of expression * ((expression list) option)
  | Function of expression * string
  | Matchee of expression * expression option * expression
and equality =
  | Equality of expression * expression
and hint =
  | Axiom
  | Induction of string
  | Nil
and declaration =
  | Prove of expression * equality * hint
  | Definition of pattern * expression
  | Variant of string * pattern list
 *)