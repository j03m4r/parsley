type parameter = Parameter of string * string
type expression =
  | Application of expression * expression
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