namespace Canopy2048

module GreedyBot =

    let rec collapse (acc:int) (stack:int list) =
        match stack with
        | [] -> acc
        | [_] -> acc
        | x::y::rest ->
            if x = y then collapse (acc+x) rest
            else collapse acc (y::rest)

    let rows (state:State) =
        state
        |> Seq.groupBy (fun x -> x.Row)
        |> Seq.map (fun (x,xs) -> 
            xs 
            |> Seq.sortBy (fun cell -> cell.Col) 
            |> Seq.map (fun cell -> cell.Value)
            |> Seq.toList)

    let columns (state:State) =
        state
        |> Seq.groupBy (fun x -> x.Col)
        |> Seq.map (fun (x,xs) -> 
            xs 
            |> Seq.sortBy (fun cell -> cell.Row) 
            |> Seq.map (fun cell -> cell.Value)
            |> Seq.toList)

    let prepare (state:State) (move:Move) =
        match move with
        | Up -> columns state
        | Down -> columns state |> Seq.map (List.rev)
        | Left -> rows state
        | Right -> rows state |> Seq.map (List.rev)

    let moves = [| Up; Down; Left; Right; |]

    let valueOf (state:State) (move:Move) =
        move |> prepare state |> Seq.map (fun x -> collapse 0 x) |> Seq.sum

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