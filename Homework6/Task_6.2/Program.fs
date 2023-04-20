module StringNumber

type StringNumberBuilder() =
    member this.Bind(s: string, f) =
        try
            let x = s |> int
            f x
        with
            | Exception -> None
            
    member this.Return x = Some(x)
    