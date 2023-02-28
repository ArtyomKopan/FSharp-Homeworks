let rec power a n =
    if n = 0 then 1
    else if n = 1 then a
    else a * (power a (n - 1))
    
// returns list [2^n, 2^n+1, …, 2^n+m]
let powers_list n m =
    let rec add_two_power acc bottom_border current_power =
        if current_power < bottom_border then acc
        else add_two_power ((List.head acc) / 2 :: acc) bottom_border (current_power - 1)
    add_two_power [power 2 (n + m)] n (n + m - 1)

let list = powers_list 1 10
printfn $"{list}"