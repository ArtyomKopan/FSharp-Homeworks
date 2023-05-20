module Rounding

type RoundingBuilder(prec: int) =
    member this.Bind(x: float, f: float -> float) =
        f (System.Math.Round(x, prec))
    member this.Return(x: float) = System.Math.Round(x, prec)