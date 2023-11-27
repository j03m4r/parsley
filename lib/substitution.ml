include Ast

type substitution = 
  | Empty
  | Singleton of string * expression (* maybe Singleton of string * substitution *)
let merge (substitution1 : substitution) (substitution2 : substitution) = 
  match (substitution1, substitution2) with
  | (Empty, Singleton ((name : string), (expr : expression))) -> Some (Empty, Singleton (name, expr))
  | (Singleton ((name : string), (expr : expression)), Empty) -> Some (Singleton (name, expr), Empty)
  | (Singleton ((name1 : string), (expr1 : expression)), Singleton ((name2 : string), (expr2 : expression))) -> begin
    if name1=name2 then (if expr1=expr2 then Some (Singleton (name1, expr1), Empty) else None) else Some (Singleton (name1, expr1), Singleton (name2, expr2))
  end
  | (Empty, Empty) -> Some (Empty, Empty)

let find (target : string) (((substitution1 : substitution), (substitution2 : substitution)) : (substitution * substitution)) : expression option =
  match (substitution1, substitution2) with
  | (Singleton ((name : string), (expr : expression)), Empty)
  | (Empty, Singleton ((name : string), (expr : expression))) -> if name=target then Some expr else None
  | (Singleton ((name1 : string), (expr1 : expression)), Singleton ((name2 : string), (expr2 : expression))) -> begin
    if name1=target then Some expr1 else (if name2=target then Some expr2 else None)
  end
  | (Empty, Empty) -> None