open Moving
open NUnit.Framework
open FsUnit

[<Test>]
let ``test 1``() =
    evalMoves (0, 0) [] |> should equal (Some (0, 0))

[<Test>]
let ``test 2``() =
    evalMoves (0, 0) [Left 5] |> should equal None
    
[<Test>]
let ``test 3``() =
    evalMoves (0, 0) [Right 5; Top 10] |> should equal (Some (5, 10))
    
[<Test>]
let ``test 4``() =
    evalMoves (0, 0) [Right 5; Bottom 10] |> should equal None