module Task1

let is_even x = abs x % 2 = 0

let even_numbers_1 list =
    List.length list - Seq.fold (+) 0 (Seq.map (fun x -> abs x % 2) list)
    
let even_numbers_2 list =
    list
    |> Seq.filter is_even
    |> Seq.length
    
let even_numbers_3 list =
    list
    |> Seq.choose (fun x -> if is_even x then Some(true) else None)
    |> Seq.filter id
    |> Seq.length
    