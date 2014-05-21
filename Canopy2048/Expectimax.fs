namespace Canopy2048

module Expectimax =
   
    let dirArray = [| Left  ; Right ; Up ; Down  ;|]

    let getcol col (grid:int[,]) = [grid.[0,col];grid.[1,col];grid.[2,col];grid.[3,col]]

    let getrow row (grid:int[,]) = [grid.[row, 0];grid.[row, 1];grid.[row, 2];grid.[row, 3]]

    let rec collapse actionworked ls = 
        // first, remove zeroe
        let rec removezeroes ls = 
            match ls with 
                | [0;0;0;0] | [0;0;0] | [0;0] | [0] -> ls
                | 0 :: xs -> actionworked := true
                             List.concat [removezeroes  xs; [0]]
                | x :: xs -> List.concat [[x]; removezeroes  xs;]
                | [] -> [] // should except
        let nozeroesls = removezeroes ls
        match nozeroesls with 
            | [0;0;0;0] | [0;0;0] | [0;0] | [0] -> nozeroesls
            | x :: y :: rst -> if (x = y && x>0) then 
                                                actionworked := true
                                                List.concat [[2*x];collapse actionworked rst; [0]]
                                          else 
                                                x :: (collapse actionworked (y::rst))
            | _ -> ls // should except


    let collapsegrid gridorig dir = 
        let grid = Array2D.copy gridorig
        let actionworked = ref false
        match dir with
            | Up -> for col in [0..3] do
                        let cl = getcol col grid |> (collapse actionworked) |> List.toArray
                        for row in [0..3] do
                            Array2D.set grid row col  cl.[row]

            | Down -> for col in [0..3] do
                        let cl = getcol col grid |> List.rev |> (collapse actionworked) |> List.rev |> List.toArray
                        for row in [0..3] do
                            Array2D.set grid row col  cl.[row]
                        
                        
            | Left -> for row in [0..3] do
                        let rw = getrow row grid |> (collapse actionworked) |> List.toArray
                        for col in [0..3] do
                            Array2D.set grid row col  rw.[col]
                        
            | Right -> for row in [0..3] do
                        let rw = getrow row grid |> List.rev |> (collapse actionworked) |> List.rev |> List.toArray
                        for col in [0..3] do
                            Array2D.set grid row col  rw.[col]

        (!actionworked,grid)


    let findZeroes (g:int[,]) = 
        [ for i in [0..3] do
                for j in  [0..3] do 
                    if (Array2D.get g i j) = 0 then yield (i,j)]


    let makeList (g:int[,]) = 
        [ for j in [0] do
                for i in  [0..3] do 
                    yield Array2D.get g (3-i) j
          for j in [1] do
                for i in  [0..3] do 
                    yield Array2D.get g i j
          for j in [2] do
                for i in  [0..3] do 
                    yield Array2D.get g (3-i) j
          for j in [3] do
                for i in  [0..3] do 
                    yield Array2D.get g i j]

    let sqr n = n * n |> float

    let scoregrid grid = 
        let empties =  grid |> findZeroes |> List.length
        let baseScore = grid |> makeList |> List.mapi (fun i x -> (sqr i)*(sqr i)* (sqr x)) |> List.sum
        let mutable score = baseScore       
        for i in [0..3] do
            let mutable maxrel = true
            for j in [0..2] do
                if grid.[i,j] > grid.[i,3] then maxrel <- false
            if maxrel then score <- score + 100.0 * (min (sqr grid.[3,3] ) 1024.0 * 1024.0) 

        score

    let avg l = 
        match l with 
        | [] -> -100000.0
        | _ -> List.average l


    let isLost g = 
        let mutable lost =  true
        for d in dirArray do
            lost <- lost && (collapsegrid g d |> fst |> not)
        lost 

    let enumeratenext g i = 
        [ for (m,n) in  findZeroes g do 
            let g2 = Array2D.copy g
            Array2D.set g2 m n i
            yield g2]

    let rec score n grid  =
        if (n = 0) then float (scoregrid grid)
        else if (isLost grid) then -100000.0
        else
            let g = Array2D.copy grid
            let genPoss g =  [for d in dirArray do 
                                    let (aw,gout) =collapsegrid g d
                                    yield gout]
            let scoreList n (g:List<int[,]>) = g |> List.map (score n) |> List.max
            
            let score2 = enumeratenext grid 2 |> List.map genPoss|> List.map (scoreList (n-1) ) |> avg
            let score4 = enumeratenext grid 4 |> List.map genPoss|> List.map (scoreList (n-1) ) |> avg
            let scoreRes = score2 * 0.9 + score4 * 0.1
            scoreRes

    let stateToArray (s: State) = 
        let g = Array2D.create 4 4 0
        s |> Seq.iter (fun c -> Array2D.set g (c.Key.Row-1) (c.Key.Col-1) c.Value)
        g

    let decide (state:State) =

        let g = stateToArray state
        let possibilities = [for d in dirArray do 
                                let (aw, gout) =collapsegrid g d
                                if aw then  yield (d,gout)] 
        let mutable scoreList = []
        for (dir, grid) in possibilities do
            //if grid.[3,3]<= 2048 then
            match grid |> findZeroes |> List.length with
            | n when n > 9 -> scoreList <- (dir,score 0 grid) :: scoreList
            | n when n > 6 -> scoreList <- (dir,score 1 grid) :: scoreList
            | n when n > 3 -> scoreList <- (dir,score 2 grid) :: scoreList
            | n when n > 1 -> scoreList <- (dir,score 3 grid) :: scoreList
            | _ -> scoreList <- (dir,score 4 grid) :: scoreList

        let bestdir = scoreList |> List.maxBy snd |> fst
        bestdir