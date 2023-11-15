type expression =
  | Application of expression * expression
  | Tuple of expression list
  | Variable of string
  | Parameter of string * string
  | Equality of expression * expression
and hint =
  | Axiom
  | Induction of string
and prove =
  | Prove
and statement =
  | Statement of prove option * expression * expression * hint option

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