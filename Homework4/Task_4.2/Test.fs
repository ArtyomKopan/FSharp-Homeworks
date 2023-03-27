open PointFree

open FsCheck

let pointFreeTest (list: int list) =
    let list' = func 2 list
    let list'' = (func' 2) list
    let list''' = (func'' 2) list
    list' = list'' && list' = list'' && list'' = list'''
    
Check.Quick pointFreeTest