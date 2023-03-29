open BracketsSequence
open FsUnit
open NUnit.Framework

[<Test>]
let test1() =
    let s = "{[()]}"
    isCorrectBracketSequence s |> should equal true
    
[<Test>]
let test2() =
    let s = "{[()}"
    isCorrectBracketSequence s |> should equal false
    
[<Test>]
let test3() =
    let s = "{ab[c()P]}"
    isCorrectBracketSequence s |> should equal true
    
[<Test>]
let test4() =
    let s = "()"
    isCorrectBracketSequence s |> should equal true
    
[<Test>]
let test5() =
    let s = "{"
    isCorrectBracketSequence s |> should equal false
    
[<Test>]
let test6() =
    let s = ""
    isCorrectBracketSequence s |> should equal true

[<Test>]
let test7() =
    let s = "{()[{}]}"
    isCorrectBracketSequence s |> should equal true