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
    let rec dq acc stack =
        match stack with
        | [] -> acc
        | [x] -> x::acc
        | x::y::rest ->
            match (x=y) with
            | true -> dq ((x+y)::acc) rest
            | false -> dq (x::acc) (y::rest)
    dq [] xs

(*
Straightforward, with duplication
*)

let apply (action:Action) (stack:Value list) =
    match action with
    | Push -> collapse stack
    | Pull -> List.rev stack |> collapse

let stackUp (state:State) =
    let extractColumn = column state
    seq { 
        for col in 1 .. 4 do
            yield!
                extractColumn col
                |> apply Push  
                |> List.mapi (fun i value -> 
                    { Row = i + 1; Col = col }, value)
    } |> Map.ofSeq

let stackDown (state:State) =
    let extractColumn = column state
    seq { 
        for col in 1 .. 4 do
            yield!
                extractColumn col
                |> apply Pull  
                |> List.mapi (fun i value -> 
                    { Row = 4 - i; Col = col }, value)
    } |> Map.ofSeq

let stackLeft (state:State) =
    let extractRow = row state
    seq { 
        for row in 1 .. 4 do
            yield!
                extractRow row
                |> apply Pull  
                |> List.mapi (fun i value -> 
                    { Row = row; Col = i + 1 }, value)
    } |> Map.ofSeq

let stackRight (state:State) =
    let extractRow = row state
    seq { 
        for row in 1 .. 4 do
            yield!
                extractRow row
                |> apply Push  
                |> List.mapi (fun i value -> 
                    { Row = row; Col = 4 - i }, value)
    } |> Map.ofSeq

let stackBy (state:State) (move:Move) =
    match move with
    | Up -> stackUp state
    | Down -> stackDown state
    | Left -> stackLeft state
    | Right -> stackRight state

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
    seq {
        for pos in 1 .. 4 do
            yield!
                pos
                |> extractor
                |> apply action
                |> rehydrator pos
        } |> Map.ofSeq

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