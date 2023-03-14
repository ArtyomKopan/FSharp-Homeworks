open Tree
open FsUnit
open NUnit.Framework

[<Test>]
let ``map for tree test 1`` () =
    let tree = Tree(1,
                Tree(2,
                     Tip(4),
                     Tip(5)),
                Tree(3,
                     Tip(6),
                     Tip(7))
                )
    let expected = Tree (2, Tree (4, Tip 8, Tip 10), Tree (6, Tip 12, Tip 14))
    let result = mapForTree ((*) 2) tree
    result |> should equal expected
    
[<Test>]
let ``map for tree test 2`` () =
    let tree = Tree(1,
                Tree(2,
                     Tip(4),
                     Tip(5)),
                Tree(3,
                     Tip(6),
                     Tip(7))
                )
    let expected = Tree (2, Tree (3, Tip 5, Tip 6), Tree (4, Tip 7, Tip 8))
    let result = mapForTree ((+) 1) tree
    result |> should equal expected
    

