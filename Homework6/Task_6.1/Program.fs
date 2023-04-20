module Rounding

type RoundingBuilder(prec: int) =
    member this.Bind(x: float, f: float -> float) =
        System.Math.Round(f x, prec)
    member this.Return(x) = x
    
