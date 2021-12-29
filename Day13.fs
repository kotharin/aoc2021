namespace Day13

module Shared =

    type Fold = 
        | Horizontal of int 
        | Vertical of int
    with 
        static member parse (line:string) =
            let v = (int)(line.Split([|'='|]).[1])
            if (line.Contains("y=")) then
                Horizontal(v)
            else
                Vertical(v)

    let transpose fold (currentX, currentY) =
        match fold with
        | Horizontal y ->
            if (currentY < y) then
                Some (currentX, currentY)
            elif (currentY = y) then
                None
            else
                // transpose
                let newY = currentY - (2*(currentY - y))
                Some (currentX,newY)
        | Vertical x ->
            if (currentX < x) then
                Some (currentX, currentY)
            elif (currentX = x) then
                None
            else
                // transpose
                let newX = currentX - (2*(currentX - x))
                Some (newX,currentY)


    // Fold paper up along horizontal at given Y
    let foldPaper fold paper =
        paper
        |> Map.fold (fun newPaper (x,y) _ ->
            // if y is less than the yFold, leave intact
        let newPoint = transpose fold (x,y)
        newPoint
        |> Option.map (fun np ->
            (Map.add np 1 newPaper)
        )
        |> Option.defaultValue newPaper
            
        ) (Map.empty)

    let parseFile lines =

        lines
        |> Array.fold (fun (paper,instructions) (line:string) ->
            if (line.Trim().Length > 0) then
                let newPaper,newIns =
                    if (line.Contains("fold")) then
                        let fold = Fold.parse line
                        paper, (List.append instructions [fold])
                    else
                        let pd = line.Split([|','|])
                        let x,y = ((int)pd.[0]), ((int)pd.[1])
                        (Map.add (x,y) 1 paper),instructions
                newPaper, newIns
            else
                paper,instructions
        ) (Map.empty,List.empty)

    // loop through the entire paper and print it
    let print maxCols maxRows paper  =

        [0..maxRows]
        |> List.iter (fun y ->
            let l =
                [0..maxCols]
                |> List.fold (fun line x ->
                    Map.tryFind (x,y) paper
                    |> Option.map (fun _ -> line + "#")
                    |> Option.defaultValue (line + ".")
                    
                ) ""
            printfn "%s" l
        )

module Part1 =
    open Shared
    open System.IO

    let solution inputFile =

        let paper, instructions =
            File.ReadAllLines inputFile
            |> parseFile
        
        let firstFold = List.head instructions

        // fold paper based on first instruction
        foldPaper firstFold paper
        |> Map.count

module Part2 =
    open Shared
    open System.IO
    let solution inputFile =

        let paper, instructions =
            File.ReadAllLines inputFile
            |> parseFile
        

        // fold paper based all instructions
        let newPaper =
            instructions
            |> List.fold (fun np ins ->
                foldPaper ins np
            ) paper
        
        print 40 7 newPaper

        "PCPHARKL"
    