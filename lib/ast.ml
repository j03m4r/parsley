type expression =
  | Variable of string
  | Application of (expression * expression)
  | Tuple of (expression list)
  | Type of string
  | Parameter of (expression * expression)
  | TypedVariable of (string * string)
  | FunctionDefintion of (expression * expression list)
  | Hint of string
  | Equality of (expression * expression * expression)

let (*prove*) cf_idempotent (h : int) =  = (cf (cf h) = cf h) (*hint: axiom *)
Equality(FunctionDefintion(Variable "cf_idempotent", [Parameter(Variable "h", Type "int")]), Equality(Application(Application (Variable "cf", Variable "h")), Application(Variable "cf", Variable "h"), Hint "axiom"))