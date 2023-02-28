let find_element list element =
    let rec find list element index =
        if List.length list = 1 && List.head list <> element then -1
        else if List.head list = element then index
        else find (List.tail list) element (index + 1)
    find list element 0
    
let list = [1;2;3;4;5]

printfn $"{find_element list 4}"
printfn $"{find_element list 6}"

