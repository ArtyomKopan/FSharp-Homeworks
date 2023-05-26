let reverse_list list =
    let rec reverse list acc =
        match list with
        | [] -> acc
        | h :: t -> reverse t (h :: acc)
    reverse list []
        
    
let list = [1; 2; 3; 4; 5]
let reversed_list = reverse_list list
printfn $"%A{reversed_list}"