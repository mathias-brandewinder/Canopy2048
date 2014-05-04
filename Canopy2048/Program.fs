namespace Canopy2048

open canopy
open runner
open System
open Expectimax

module program = 

    let lost () =
        match someElement ".game-message.game-over" with
        | None -> false
        | Some(_) -> true

    let won () =
        match someElement ".game-message.game-won" with
        | None -> false
        | Some(_) -> true

    let state () = 
        elements ".tile"
        |> List.map(fun tile ->
            let classes = tile.GetAttribute("class").Split(' ')
            let pointClass = classes |> Array.find(fun classs -> classs.StartsWith("tile-") && not (classs.StartsWith("tile-position-")))
            let point = pointClass.Split('-').[1] |> int
            let rowColumnClass = classes |> Array.find(fun classs -> classs.StartsWith("tile-position-"))
            let column = rowColumnClass.Split('-').[2] |> int
            let row = rowColumnClass.Split([|'-'|]).[3] |> int
            { Row = row; Col = column; Value = point })
        // removing the duplicates due to merges:
        // group cell by identical row, col
        // and keep the one with largest value,
        // hacky but works...
        |> Seq.groupBy (fun x -> x.Row, x.Col)
        |> Seq.map (fun ((row,col),cells) -> 
            cells |> Seq.maxBy (fun cell -> cell.Value))
        |> Seq.toList

    let stateToArray (s : Cell list) = 
        let g = Array2D.create 4 4 0
        s |> List.map (fun c -> Array2D.set g (c.Row-1) (c.Col-1) c.Value) |> ignore
        g

    let play (move:Move) =
        match move with
        | Up -> press up
        | Down -> press down
        | Right -> press right
        | Left -> press left

    let showDecision (move:Move) = 
        printfn "%A" move
        move

    "starting a game of 2048" &&& fun _ ->
    
        start chrome
        url @"http://gabrielecirulli.github.io/2048/"

        let rec nextMove () =
            if lost () then 
                printfn "Game over: loss!"
            elif won () then
                printfn "Game over: win!"                
            else          
                printfn "Thinking..."
                state ()
                |> stateToArray
                |> Expectimax.decide
                |> showDecision
                |> play
                nextMove ()

        nextMove ()

    //run all tests
    run()

    printfn "press [enter] to exit"
    System.Console.ReadLine() |> ignore

    quit()