namespace Canopy2048

open Canopy2048.Game

module BellmanBot =

    let moves = [| Up; Down; Left; Right; |]

    let nextStates (state:State) (move:Move) =
        let _,next = execute state move
        let emptyCells = empty next
        seq { for key in emptyCells do
                yield (next |> Map.add key 2)
                yield (next |> Map.add key 4) }

    let valueOf (state:State) (move:Move) =
        
        let score,updated = execute state move
        
        let empties = empty updated
        match (Seq.isEmpty empties) with
        | true  -> score
        | false ->
            seq {
                for choice in moves do
                    let results = seq {
                        for key in empties do 
                            let ``2 added`` = updated |> Map.add key 2 
                            for choice in moves do
                                yield (execute ``2 added`` choice |> fst)
                            let ``4 added`` = updated |> Map.add key 4
                            for choice in moves do
                                yield (execute ``4 added`` choice |> fst)
                        } 
                    yield results |> Seq.min
            } |> Seq.max
    
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