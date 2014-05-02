namespace Canopy2048

open canopy
open runner
open System
open Canopy2048.GreedyBot

module program = 

    let finished () =
        match someElement ".game-message.game-over" with
        | None -> false
        | Some(_) -> true

    let cell col row =
        let heading = (element ".heading")
        let container = (element ".tile-container")
        let mergedCss = sprintf ".tile-position-%i-%i.tile-merged" col row                
        match (someElementWithin mergedCss container) with
        | Some(x) -> Some { Col=col; Row=row; Value=int (x.Text) }
        | None -> 
            let newCss = sprintf ".tile-position-%i-%i.tile-new" col row
            match (someElementWithin mergedCss container) with
            | Some(x) -> Some { Col=col; Row=row; Value=int (x.Text) }
            | None -> 
                let css = sprintf ".tile-position-%i-%i" col row
                match (someElementWithin css container) with
                | Some(x) -> Some { Col=col; Row=row; Value=int (x.Text) }
                | None -> None
    
    let state () =
        [   for col in 1 .. 4 do
                for row in 1 .. 4 do
                    yield cell col row ]
        |> List.choose id

    let moves = [| Up; Down; Left; Right; |]
    let rng = System.Random ()
    
    let decide (state:State) =
        moves.[rng.Next(0,4)]

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
        url "http://gabrielecirulli.github.io/2048/"

        let rec nextMove () =
            if finished () then 
                printfn "Game over"
                ignore ()
            else          
                printfn "Thinking"
                state ()
                |> GreedyBot.decide
                |> showDecision
                |> play
                nextMove ()

        nextMove ()

    //run all tests
    run()

    printfn "press [enter] to exit"
    System.Console.ReadLine() |> ignore

    quit()