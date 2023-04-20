module Prime

open System.Numerics

let isPrime n =
    let rec isDivideForSomething n x =
        // проверяет, существует ли такое число 2 <= y <= x : y | n
        if n % x = 0I then true
        else if x <= 2I then false
        else isDivideForSomething n (x - 1I)
    if n <= 1I then false
    else if n = 2I then true
    else not (isDivideForSomething n (bigint (ceil (sqrt (double n)))))
    
let rec getNextPrime x =
    if isPrime (x + 1I) then x + 1I
    else getNextPrime (x + 1I)
    
let primeNumbers = Seq.unfold (fun x -> Some(x, getNextPrime x)) 2I


for x in Seq.take 100 primeNumbers do
    printfn $"{x}"
        
