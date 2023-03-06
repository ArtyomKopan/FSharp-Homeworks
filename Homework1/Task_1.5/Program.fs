let find_element list element =
    let rec find list element index =
        if List.length list = 1 && List.head list <> element then None
        else if List.head list = element then Some(index)
        else find (List.tail list) element (index + 1)
    find list element 0
    
let list = [1; 2; 3; 4; 5]

let printIndex list element =
    let position = find_element list element
    if position.IsNone then printfn "Элемент не найден"
    else printfn $"Индекс элемента = {position.Value}"

printIndex list 4
printIndex list 6