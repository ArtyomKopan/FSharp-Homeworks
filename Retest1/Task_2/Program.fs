module Drawing

let drawX n =
  let size = 2 * n - 1
  String.init size (fun lineIndex ->
    let k = min lineIndex (size - lineIndex - 1)
    String.init size (fun columnIndex
                       -> if columnIndex = k || columnIndex = size - 1 - k then "*"
                          else " ") + "\n")
  |> printf "%s"
  
// usage example
drawX 5