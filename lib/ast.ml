type expression =
  | Variable of string
  | Application of expression * expression
  | Tuple of expression list
  | Match of expression * (expression * expression) list

type statement =
  | Proof of string * expression * hint option
  | TypeDef of string * expression
  | FuncDef of string * expression * expression