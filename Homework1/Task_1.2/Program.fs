let fibonacci n =
    let rec fib n current_num f1 f2 =
        if current_num = n then f1 + f2
        else fib n (current_num + 1) f2 (f1 + f2)
    if n = 0 then 0
    else if n = 1 then 1
    else fib n 2 0 1
    
printfn $"%A{List.map fibonacci [1..20]}"