module Lambda

type LambdaExpression =
    | Var of string
    | Abstraction of string * LambdaExpression
    | Application of LambdaExpression * LambdaExpression
    
type ReplaceStatus = Yes | No
    
let rec getFreeVars expression =
    match expression with
    | Var v -> Set.singleton v
    | Abstraction (v, expr) -> Set.remove v (getFreeVars expr)
    | Application (expr1, expr2) -> Set.union (getFreeVars expr1) (getFreeVars expr2)
  
let generateVarName var = var + "'" 
  
let rec alphaConversion expression var newVar =
    match expression with
    | Var v -> if v = var then Var(newVar) else Var(v)
    | Application (expr1, expr2) -> Application (alphaConversion expr1 var newVar,
                           alphaConversion expr2 var newVar)
    | Abstraction (v, e) -> if v = var then
                                Abstraction (newVar, alphaConversion e v newVar)
                            elif Set.contains newVar (getFreeVars e) then
                                let v' = generateVarName v
                                Abstraction (
                                    v',
                                    alphaConversion (alphaConversion (Abstraction (v', e)) v v') var newVar 
                                )
                            else Abstraction (v, alphaConversion e var newVar)

let alphaRename expression var =
    if Set.contains var (getFreeVars expression) then
        let newVar = generateVarName var
        (alphaConversion expression var newVar, newVar)
    else (expression, var)

let rec replaceTherm expression varName therm =
    match expression with
    | Var v when v = varName -> (therm, Yes)
    | Var _ -> (expression, No)
    | Application (expr1, expr2) -> 
        let result1 = replaceTherm expr1 varName therm
        let result2 = replaceTherm expr2 varName therm
        if snd result1 = Yes then (Application(fst result1, expr2), Yes)
        elif snd result2 = Yes then (Application(expr1, fst result2), Yes)
        else (Application(expr1, expr2), No)
    | Abstraction (v, expr) ->
        if v = varName then replaceTherm expr varName therm
        else (expression, No)
        
let rec normalize expression =
    match expression with
    | Var _ -> expression
    | Abstraction (v, expr) -> Abstraction (v, normalize expr)
    | Application (Abstraction (v, expr1), expr2) -> betaConversion v expr1 expr2
    | Application (expr1, expr2) -> Application (normalize expr1, normalize expr2)
and
  betaConversion v expression1 expression2 =
      let expression1' = alphaRename expression1 v
      normalize (fst (replaceTherm (fst expression1') (snd expression1') expression2))
    
let rec lambdaToString expression =
    match expression with
    | Var v -> v
    | Abstraction (v, expr) -> $"λ{v}." + lambdaToString expr
    | Application (expr1, expr2) -> "(" + lambdaToString expr1+ ") " + lambdaToString expr2 
