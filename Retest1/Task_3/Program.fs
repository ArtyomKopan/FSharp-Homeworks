module Moving

type movement = 
  | Left of int 
  | Right of int 
  | Top of int 
  | Bottom of int


type OptionBuilder() =
  member _.Bind(v, f) = Option.bind f v
  member _.Return v = Some v
  member _.Zero () = None

let opt = OptionBuilder()

let evalMoves startPoint movesList =
    let rec move (x, y) moves =
        match moves with
        | [] -> opt { return (x, y) }
        | Left dx :: ms ->
            let newX = x - dx
            if newX < 0 then opt { if false then
                                       return (x, y) } //это вызов Zero
            else move (newX, y) ms
        | Right dx :: ms ->
            let newX = x + dx
            if newX < 0 then opt { if false then
                                       return (x, y) }
            else move (newX, y) ms
        | Top dy :: ms ->
            let newY = y + dy
            if newY < 0 then opt { if false then
                                       return (x, y) }
            else move (x, newY) ms
        | Bottom dy :: ms ->
            let newY = y - dy
            if newY < 0 then opt { if false then
                                       return (x, y) }
            else move (x, newY) ms
    
    move startPoint movesList
    