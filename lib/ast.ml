type expression =
  | Variable of string
  | Application of (expression * expression)
  | Tuple of (expression list)