module Supermap

let supermap (functions: ('a -> 'b) list) (list: 'a list) : 'b list =
    let rec apply_k_func (functions: ('a -> 'b) list) (list: 'a list) k current_results =
        let k_result = List.map (fun f -> f list[k]) functions
        match k with
        | 0 -> k_result :: current_results
        | _ -> apply_k_func functions list (k - 1) (k_result :: current_results)
                
    let n = List.length functions
    let result = apply_k_func functions list n []
    List.concat result
    