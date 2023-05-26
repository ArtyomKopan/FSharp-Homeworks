let fibonacci n =
    let rec fib n f1 f2 =
        match n with
        | 0 -> 0
        | 1 -> 1
        | 2 -> f1 + f2
        | _ -> fib (n - 1) f2 (f1 + f2)
    fib n 0 1
    
printfn $"%A{List.map fibonacci [0..20]}"