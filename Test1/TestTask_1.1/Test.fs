open Supermap
open FsUnit
open NUnit.Framework

[<Test>]
let ``test 1``() =
   let l = supermap [sin; cos] [1.0; 2.0; 3.0]
   let l' = List.map (fun (x: float) -> System.Math.Round(x, 3)) l
   printfn $"%A{l'}"
   l' |> should equal (List.map (fun (x: float) -> System.Math.Round(x, 3)) [sin 1.0; cos 1.0; sin 2.0; cos 2.0; sin 3.0; cos 3.0])
