module ArithmeticTree

type ArithmeticOperation =
    | Plus
    | Minus
    | Product
    | Divide

type ArithmeticTree<'a> =
    | Value of 'a
    | ArithmeticTree of ArithmeticOperation * ArithmeticTree<'a> * ArithmeticTree<'a>

let rec computeExpression tree =
    match tree with
    | Value x -> x
    | ArithmeticTree(op, left, right)
         -> match op with
            | Plus -> computeExpression left + computeExpression right
            | Minus -> computeExpression left - computeExpression right
            | Product -> computeExpression left * computeExpression right
            | Divide -> computeExpression left / computeExpression right