namespace Canopy2048

open Canopy2048.Game

module BellmanBot =

    let moves = [| Up; Down; Left; Right; |]

        
    let rng = System.Random()
    let shuffle (arr:'a []) =
        let l = arr.Length
        for i in (l-1) .. -1 .. 1 do
            let temp = arr.[i]
            let j = rng.Next(0,i+1)
            arr.[i] <- arr.[j]
            arr.[j] <- temp
        arr

    let p2, p4 = 0.9, 0.1

    let nextStates (state:State) =
        let openSpaces = empty state
        let count = openSpaces |> Seq.length |> float
        [ for pos in openSpaces do
            yield p2/count, state |> Map.add pos 2
            yield p4/count, state |> Map.add pos 4 ]
                    
    let rec scoreOf (state:State) (move:Move) (depth:int) =
        let score,next = execute state move
        if next = state
        then 0.
        elif depth = 0 
        then (float score)
        else 
            let nexts = nextStates next
            let d = depth - 1
            let extra = 
                nexts 
                |> Seq.sumBy (fun (p,s) -> p * (scoreOf s (bestMove s d) d))
            float score + extra

    and bestMove (state:State) (depth:int) =
        let validMoves = 
            moves 
            |> Seq.filter (fun move -> not (invariant state move))
        match (Seq.isEmpty validMoves) with
        | true -> Up // totally arbitrary
        | false ->
            validMoves
            |> Seq.maxBy (fun move -> 
                scoreOf state move depth)

    let decide (state:State) = 
        let validMoves = 
            moves 
            |> Seq.filter (fun move -> 
                not (invariant state move))
        match (Seq.length validMoves) with
        | 0 -> Up // totally arbitrary
        | 1 -> (Seq.head validMoves)
        | _ ->  
            let empties = empty state |> Seq.length
            let depth = 
                if empties > 4 then 2 
                elif empties > 1 then 3
                else 4
            bestMove state depth