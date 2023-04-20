open StringNumber
open FsUnit
open NUnit.Framework

[<Test>]
let ``test 1``() =
    let calculate = StringNumberBuilder()
    let result = calculate {
        let! x = "1"
        let! y = "Ъ"
        let z = x + y
        return z
    }
    result |> should equal None

[<Test>]
let ``test 2``() =
    let calculate = StringNumberBuilder()
    let result = calculate {
        let! x = "1"
        let! y = "2"
        let z = x + y
        return z
    }
    result.Value |> should equal 3
