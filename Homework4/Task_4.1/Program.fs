module BracketsSequence

let isCorrectBracketSequence s =
    let brackets = ['('; ')'; '['; ']'; '{'; '}']
    let leftBrackets = ['('; '['; '{']
    let rightBrackets = [')'; ']'; '}']
    
    let getPairBracket (bracket: char) =
        if (List.contains bracket leftBrackets) then
            rightBrackets[List.findIndex (fun x -> x = bracket) leftBrackets]
        else
            leftBrackets[List.findIndex (fun x -> x = bracket) rightBrackets]
    
    // функции для стека
    let push stack element =
        element :: stack
    
    let top stack =
        match stack with
        | [] -> None
        | h :: _t -> Some(h)
        
    let pop stack =
        match stack with
        | [] -> []
        | _h :: t -> t
        
    let sequence = String.filter (fun ch -> List.contains ch brackets) s
    
    let rec bracketsCheck stack seq index =
        if (index = String.length seq) then List.isEmpty stack
        else
            match seq[index] with
            | bracket when List.contains bracket leftBrackets ->
                bracketsCheck (push stack bracket) seq (index + 1)
            | bracket ->
                let prevBracket = top stack
                if prevBracket.IsSome && prevBracket.Value = getPairBracket bracket then
                    bracketsCheck (pop stack) seq (index + 1)
                else
                    false
                    
    bracketsCheck [] sequence 0
    
    