open PointFree

open FsCheck

let pointFreeTest (l: int list) =
    let list = func 2 l
    let list' = func' 2 l
    let list'' = func'' 2 l
    let list''' = func''' 2 l  
    list = list' && list = list'' && list = list''' &&
        list' = list'' && list' = list''' && list'' = list'''
    
Check.Quick pointFreeTest