module Lambda
 
type LambdaExpression =
    | Var of string
    | Abstraction of string * LambdaExpression
    | Application of LambdaExpression * LambdaExpression
    
let rec getFreeVars expression =
    match expression with
    | Var v -> Set.singleton v
    | Abstraction (v, expr) -> Set.remove v (getFreeVars expr)
    | Application (expr1, expr2) -> Set.union (getFreeVars expr1) (getFreeVars expr2)
  
let generateVarName var = var + "'" 
  
let rec alphaConversion expression var newVar =
    match expression with
    | Var v -> Var (if v = var then newVar else v)
    | Application (expr1, expr2) -> Application (alphaConversion expr1 var newVar,
                           alphaConversion expr2 var newVar)
    | Abstraction (v, e) -> 
        if v = var then
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
 
let rec replaceTerm expression varName term =
    match expression with
    | Var v when v = varName -> term
    | Var _ -> expression
    | Abstraction (v, expr) when v = varName -> Abstraction (v, expr)
    | Abstraction (v, expr) ->
        if (not (Set.contains v (getFreeVars term)) || not (Set.contains varName (getFreeVars expr))) then
            let replacedExpr = replaceTerm expr varName term
            Abstraction (v, replacedExpr)
        else
            let renamedExpr, v' = alphaRename expr v
            let replacedExpr = replaceTerm renamedExpr varName term
            Abstraction(v', replacedExpr)
    | Application (expr1, expr2) ->
        Application(replaceTerm expr1 varName term, replaceTerm expr2 varName term)
        
let rec normalize expression =
    match expression with
    | Var _ -> expression
    | Abstraction (v, expr) -> Abstraction (v, normalize expr)
    | Application (Abstraction (v, expr1), expr2) ->
        normalize (replaceTerm expr1 v expr2)
    | Application (expr1, expr2) ->
        Application (normalize expr1, normalize expr2)
    
let rec lambdaToString expression =
    match expression with
    | Var v -> v
    | Abstraction (v, expr) -> $"λ{v}.{lambdaToString expr}"
    | Application (expr1, expr2) -> $"{lambdaToString expr1} {lambdaToString expr2}"