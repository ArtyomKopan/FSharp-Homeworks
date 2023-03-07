module Tree

type Tree<'a> =
    | Tree of 'a * Tree<'a> * Tree<'a>
    | Tip of 'a

let rec size tree =
    match tree with
    | Tree(_, l, r) -> 1 + size l + size r
    | Tip _ -> 1
    
    
let rec mapForTree action tree =
    match tree with
    | Tip i -> Tip (action i)
    | Tree(i, l, r) -> Tree(action i, mapForTree action l, mapForTree action r)

    