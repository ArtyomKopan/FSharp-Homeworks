type OptionBuilder() =
  member _.Bind(v, f) = Option.bind f v
  member _.Return v = Some v
  member _.Zero () = None

type binOp = 
  | Add 
  | Sub 
  | Mul 
  | Div
type expr = 
  | Var of string 
  | Const of int 
  | BinOp of binOp * expr * expr
  
let rec compute (substitution: Map<string, int>) (expression: expr) : int option =
   let opt = OptionBuilder()
   match expression with
   | Const v -> opt { return v }
   | Var v ->
     opt {
         return substitution[v]
     }
   | BinOp (op, arg1, arg2) ->
     match op with
     | Add -> compute (ar)