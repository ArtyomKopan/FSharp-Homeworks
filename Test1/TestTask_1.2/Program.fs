let printRhombus n =
    let k_odd_number k =
        2 * k - 1
        
    let rec print_k_symbols symbol k =
        match k with
        | 1 -> printf "%s" symbol
        | _ ->
            printf "%s" symbol
            print_k_symbols symbol (k - 1)
        
    let rec printRhombusLayer k isReverse =
        match isReverse with
        | false ->
            match k with
                | 1 ->
                    print_k_symbols " " (n - 1)
                    printf "*\n"
                    printRhombusLayer 2 false
                | m when m < n ->
                    print_k_symbols " " (n - m)
                    print_k_symbols "*" 1
                    print_k_symbols " " (k_odd_number (m - 1))
                    printf "*\n"
                    printRhombusLayer (m + 1) false
                | n ->
                    print_k_symbols "*" 1
                    print_k_symbols " " (k_odd_number (n - 1))
                    printf "*\n"
                    printRhombusLayer (n - 1) true
                
        | true ->
            match k with
                | 1 ->
                    print_k_symbols " " (n - 1)
                    printf "*\n"
                | m ->
                    print_k_symbols " " (n - m)
                    print_k_symbols "*" 1
                    print_k_symbols " " (k_odd_number (m - 1))
                    printf "*\n"
                    printRhombusLayer (m - 1) true
                    
    printRhombusLayer 1 false
                     
printRhombus 6