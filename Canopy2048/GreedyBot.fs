namespace Canopy2048

open Canopy2048.Game

module GreedyBot =

    let moves = [| Up; Down; Left; Right; |]

    let valueOf (state:State) (move:Move) =
        execute state move |> fst

    let rng = System.Random()
    let shuffle (arr:'a []) =
        let l = arr.Length
        for i in (l-1) .. -1 .. 1 do
            let temp = arr.[i]
            let j = rng.Next(0,i+1)
            arr.[i] <- arr.[j]
            arr.[j] <- temp
        arr

    let decide (state:State) =
        moves |> shuffle |> Array.maxBy (valueOf state)