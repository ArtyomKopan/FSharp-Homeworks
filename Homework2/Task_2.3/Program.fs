open ArithmeticTree
open FsUnit
open NUnit.Framework

[<Test>]
let ``test of easy tree`` () =
    // 5 + 3 * 2
    let tree =
        ArithmeticTree(
            Plus,
            Value(5),
            ArithmeticTree(
                Product,
                Value(3),
                Value(2)
            )
        )
    
    computeExpression tree |> should equal 11

[<Test>]
let ``test of difficult tree`` () =
    // (3 + 5) / 2 - 1 + 5 * (2 + 3)
    let tree =
        ArithmeticTree(
            Plus,
            ArithmeticTree(
                Minus,
                ArithmeticTree(
                    Divide,
                    ArithmeticTree(
                        Plus,
                        Value(3),
                        Value(5)
                    ),
                    Value(2)
                ),
                Value(1)
            ),
            ArithmeticTree(
                Product,
                Value(5),
                ArithmeticTree(
                    Plus,
                    Value(2),
                    Value(3)
                )
            )
        )
    
    computeExpression tree |> should equal 28