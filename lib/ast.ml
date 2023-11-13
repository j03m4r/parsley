type expression =
  | Variable of string
  | Application of expression * expression
  | Tuple of expression list

type statement =
  | Prove of string * expression * hint option

and hint =
  | Axiom