open Rounding
open FsUnit
open NUnit.Framework

[<Test>]
let ``test 1``() =
    let rounding prec = RoundingBuilder(prec)
    let r = rounding 3 {
        let! a = 4.0
        let! b = 3.0
        return a / b
    }
    r |> should equal 1.333

[<Test>]
let ``test2``() =
    let rounding prec = RoundingBuilder(prec)
    let r = rounding 3 {
        let! a = 2.0 / 12.0
        let! b = 3.5
        return a / b
    }
    r |> should equal 0.048
    
[<Test>]
let ``test 3``() =
    let rounding prec = RoundingBuilder(prec)
    let r = rounding 3 { return 1.0 / 3.0 }
    r |> should equal 0.333