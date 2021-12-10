namespace Day7

module Part1 = 
    open System
    open System.IO

    let cost multiplier (target:int) sources  =
        sources
        |> Array.map(fun source ->
            // Get number of crabs at source
            let num = Map.find source multiplier
            num * Math.Abs(source - target))
        |> Array.sum

    let solution inputFile =

        let line = File.ReadAllText inputFile

        let crabAtApositionsMap =
            line.Trim().Split([|','|])
            |> Array.map int
            |> Array.groupBy id
            |> Array.map (fun (pos,occupants) -> 
                pos,Array.length occupants
            )
            |> Map.ofArray

        let crabAtPositions =
            crabAtApositionsMap
            |> Map.toArray
            |> Array.map fst

        let min,max =
            (Array.min crabAtPositions),(Array.max crabAtPositions)

        [|min..max|]
        |> Array.map (fun target ->
            let cost = cost crabAtApositionsMap target crabAtPositions
            //printfn "cost;%i,i:%i" cost target
            cost,target
        )
        |> Array.minBy (fst)
        |> fst

module Part2 =
    open System
    open System.IO

    let cost multiplier (target:int) sources  =
        sources
        |> Array.map(fun source ->
            // Get number of crabs at source
            let num = Map.find source multiplier
            let n = Math.Abs(source - target)
            let cost = n*(n+1)/2
            num * cost)
        |> Array.sum

    let solution inputFile =
        let line = File.ReadAllText inputFile

        let crabAtApositionsMap =
            line.Trim().Split([|','|])
            |> Array.map int
            |> Array.groupBy id
            |> Array.map (fun (pos,occupants) -> 
                pos,Array.length occupants
            )
            |> Map.ofArray

        let crabAtPositions =
            crabAtApositionsMap
            |> Map.toArray
            |> Array.map fst

        let min,max =
            (Array.min crabAtPositions),(Array.max crabAtPositions)

        [|min..max|]
        |> Array.map (fun target ->
            let cost = cost crabAtApositionsMap target crabAtPositions
            //printfn "cost;%i,i:%i" cost target
            cost,target
        )
        |> Array.minBy (fst)
        |> fst
    