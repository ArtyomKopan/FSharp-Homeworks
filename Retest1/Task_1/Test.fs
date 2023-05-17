open MapiRepeat
open NUnit.Framework
open FsUnit

[<Test>]
let ``test 3``() =
    let l = [1; 1; 1; 1]
    let l' = mapiRepeat (fun index element -> index * element) 3 l
    l' |> should equal [0; 1; 8; 27]
    
[<Test>]
let ``test 1``() =
    let l = [1; 1; 1; 1]
    let l' = mapiRepeat (fun index element -> index * element) 1 l
    l' |> should equal (List.mapi (fun index element -> index * element) l)
    
[<Test>]
let ``test negative n``() =
    let l = [1; 1; 1; 1]
    (fun () -> (mapiRepeat (fun i e -> i + e) -1 l) |> ignore) |> should throw typeof<System.Exception>
   
[<Test>]
let ``test n = 0``() =
    let l = [1; 1; 1; 1]
    let l' = mapiRepeat (fun index element -> index * element) 0 l
    l' |> should equal l