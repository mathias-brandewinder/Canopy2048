type Move = Up | Down | Left | Right

type Pos = { Row:int; Col:int; }

type Value = int

type State = Map<Pos,Value>

type Action = Push | Pull

type Direction = Row | Column

let column (state:State) (c:int) =
    state
    |> Seq.filter (fun kv -> kv.Key.Col = c)
    |> Seq.sortBy (fun kv -> kv.Key.Row)
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.toList

let row (state:State) (r:int) =
    state
    |> Seq.filter (fun kv -> kv.Key.Row = r)
    |> Seq.sortBy (fun kv -> kv.Key.Col)
    |> Seq.map (fun kv -> kv.Value)
    |> Seq.toList

let collapse (xs:int list) =
    let rec dq (score,acc) stack =
        match stack with
        | [] -> (score,acc)
        | [x] -> (score,x::acc)
        | x::y::rest ->
            match (x=y) with
            | true -> dq (score+x,(x+y)::acc) rest
            | false -> dq (score,x::acc) (y::rest)
    dq (0,[]) xs


let apply (action:Action) (stack:Value list) =
    match action with
    | Push -> collapse stack
    | Pull -> List.rev stack |> collapse

(*
Silly refactoring version
*)

let extractBy (state:State) (dir:Direction) =
    match dir with
    | Row    -> row state 
    | Column -> column state

let key (act:Action) (dir:Direction) (pos:int) =
    fun i ->
        match (act,dir) with
        | (Pull,Column) -> { Row = i + 1; Col = pos }
        | (Push,Column) -> { Row = 4 - i; Col = pos }
        | (Pull,Row)    -> { Row = pos; Col = i + 1 }
        | (Push,Row)    -> { Row = pos; Col = 4 - i }

let rehydrate (dir:Direction) (act:Action) (pos:int) (stack: Value list) =
    let keyer = key act dir pos
    stack |> List.mapi (fun i value -> keyer i, value)

let orientation (move:Move) =
    match move with
    | Up -> Column, Pull
    | Down -> Column, Push
    | Left -> Row, Pull
    | Right -> Row, Push

let execute (state:State) (move:Move) =
    let dir,action = orientation move
    let extractor = extractBy state dir
    let rehydrator = rehydrate dir action 
    let scores,rows = 
        [   for pos in 1 .. 4 do
                yield
                    pos
                    |> extractor
                    |> apply action ]
        |> List.unzip
    scores |> Seq.sum, // total score
    rows 
    |> List.mapi (fun i xs -> rehydrator (i+1) xs) 
    |> List.concat 
    |> Map.ofList // reconstructed state

(*
Validation on a small example

    2  8  _  _
    2  _  _  _
    4  _  _  32
    _  16 _  _
*)

let test = 
    Map.empty
    |> Map.add { Row = 1; Col = 1; } 2
    |> Map.add { Row = 2; Col = 1; } 2
    |> Map.add { Row = 3; Col = 1; } 4
    |> Map.add { Row = 1; Col = 2; } 8
    |> Map.add { Row = 4; Col = 2; } 16
    |> Map.add { Row = 3; Col = 4; } 32

let render (state:State) =
    for row in 1 .. 4 do
        printfn ""
        for col in 1 .. 4 do
            match (Map.tryFind { Row = row; Col = col } state) with
            | None -> printf " . "
            | Some(v) -> printf "%2i " v   
    printfn ""