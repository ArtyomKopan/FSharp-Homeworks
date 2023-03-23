open Lambda
open FsUnit
open NUnit.Framework

[<Test>]
let ``test 1``() =
    let expr = Application(Abstraction("z", Application(Abstraction("x", Application(Var "x", Var "y")), Var "z")), Var "w")
    lambdaToString (normalize expr) |> should equal "(w) y" 

[<Test>]
let ``test 2``() =
    let expr = Application(Abstraction("x", Application(Var "x", Var "y")), Var "z")
    lambdaToString (normalize expr) |> should equal "(z) y"
    
[<Test>]
let ``test 3``() =
    let expr = Application(Abstraction("x", Var "y"), Var "z")
    lambdaToString (normalize expr) |> should equal "y"
    
[<Test>]
let ``test 4``() =
    let expr = Application(Abstraction("x", Application(Abstraction("y", Application(Var "x", Var "y")), Var "z")),
        Var "w")
    lambdaToString (normalize expr) |> should equal "(w) z"