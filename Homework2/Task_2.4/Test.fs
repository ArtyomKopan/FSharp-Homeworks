open NUnit.Framework
open Prime
open FsUnit

[<Test>]
let ``correct first 10 prime numbers`` () =
    let listPrimes = [2I; 3I; 5I; 7I; 11I; 13I; 17I; 19I; 23I; 29I] |> Seq.ofList 
    let seqPrimes = Seq.take 10 primeNumbers
    listPrimes |> should equal seqPrimes
    
[<Test>]
let ``correct big prime number`` () =
    let bigPrimeNumber = [for i in Seq.take 500 primeNumbers -> i][499]
    bigPrimeNumber |> should equal 3571I
    