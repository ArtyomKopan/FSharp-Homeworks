open Lambda
open FsUnit
open Microsoft.FSharp.Core
open NUnit.Framework

[<Test>]
let ``test 1``() =
    let expr = Application(Abstraction("z", Application(Abstraction("x", Application(Var "x", Var "y")), Var "z")), Var "w")
    lambdaToString (normalize expr) |> should equal "w y" 

[<Test>]
let ``test 2``() =
    let expr = Application(Abstraction("x", Application(Var "x", Var "y")), Var "z")
    lambdaToString (normalize expr) |> should equal "z y"
    
[<Test>]
let ``test 3``() =
    let expr = Application(Abstraction("x", Var "y"), Var "z")
    lambdaToString (normalize expr) |> should equal "y"
    
[<Test>]
let ``test 4``() =
    let expr = Application(Abstraction("x", Application(Abstraction("y", Application(Var "x", Var "y")), Var "z")),
        Var "w")
    lambdaToString (normalize expr) |> should equal "w z"
    
[<Test>]
let ``test 5``() =
    let expr = Application(Abstraction("x", Abstraction("x", Var "x")), Var "y")
    lambdaToString (normalize expr) |> should equal "λx.x"
    
[<Test>]
let ``test 6``() =
    let expr = Application(Abstraction("x", Abstraction("x''", Application(Var "x", Var "x'"))), Var "y")
    lambdaToString (normalize expr) |> should equal "λx''.y x'"
    
[<Test>]
let ``test 7``() =
    let expr = Application(Abstraction("x", Application(Var "x", Var "x'")), Var "y")
    lambdaToString (normalize expr) |> should equal "y x'"
    
[<Test>]
let ``test 8``() =
    let expr = Application(Abstraction("x", Var "x"), Var "x")
    lambdaToString (normalize expr) |> should equal "x"
    
[<Test>]
let ``test 9``() =
    let expr = Application(Abstraction("c", Application(Var "c", Var "c")),
                           Abstraction("a", Var "a"))
    lambdaToString (normalize expr) |> should equal "λa.a"

[<Test>]
let ``test 10``() =
    let expr = Application(Application(Abstraction("x", Application(Var "x", Var "x")), Var "x"), Var "x")
    lambdaToString (normalize expr) |> should equal "x x x"
    
[<Test>]
let ``test 11``() =
    let expr = Application(Abstraction("x", Var "x"), Var "x")
    alphaRename expr "x" |> should equal (Application(Abstraction("x'", Var "x'"), Var "x"), "x'")