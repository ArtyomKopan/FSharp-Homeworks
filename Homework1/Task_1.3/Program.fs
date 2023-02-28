let rec reverse_list list = 
    let len = List.length list - 1
    match List.length list with
    | 0 -> []
    | 1 -> list
    | _ -> list.[len] :: reverse_list (List.take len list)
    
let list = [1; 2; 3; 4; 5]
let reversed_list = reverse_list list
printfn $"{reversed_list}"