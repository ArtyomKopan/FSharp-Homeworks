module MapiRepeat

exception InvalidArgumentException of string

let mapiRepeat f n list =
    if n < 0 then
        failwith "Число применений функции должно быть положительным!"
    
    let rec applyMapi k f list =
        match k with
        | 0 -> list
        | 1 -> List.mapi f list
        | _ -> applyMapi (k - 1) f (List.mapi f list)
    
    applyMapi n f list
