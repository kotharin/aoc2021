namespace Day9

module Shared =
    open System
    open System.IO

    let getAdjacentPoints maxRow maxCol row col =
        [(row - 1,col);
         (row, col + 1);
         (row + 1, col);
         (row,col - 1)
        ]
        |> List.filter (fun (x,y) ->
            (x <= maxRow) && (x>=0) && (y <= maxCol) && (y >= 0)
        )

    let isSmallestComparedToAdjacent points adjacentPoints row col =
        // get the value for the current point
        let pointValue =
            Map.find (row,col) points

        // see if there is any point that is smaller than the current point
        (adjacentPoints
        |> List.filter (fun (x,y) ->
            // get value of adjacent point and compare to current
            (Map.find (x,y) points) <= pointValue
        )
        |> List.length) = 0
        
    
    let getMinimumPoints points maxRow maxCol =
        // loop through the rows and columns
        [0..maxRow]
        |> List.fold (fun (state1,state2) row ->
            // get all the columns
            [0..maxCol]
            |> List.fold (fun (innerState1,innerState2) col ->
                // get list of adjacent points
                let adjacent = getAdjacentPoints maxRow maxCol row col
                // check if this is the minimum point compared to the adjacent points
                if (isSmallestComparedToAdjacent points adjacent row col) then
                    let minVal = Map.find (row,col) points
                    (minVal::innerState1),((row,col)::innerState2)
                else
                    innerState1,innerState2
            )(state1,state2)
        ) (List.empty,List.empty)

    let getAllPointsMap (lines:string array) =
        // loop through and get all the points into a Map
        lines
        |> Array.fold (fun (state,rowNum) line ->
            // loop through the columns
            let newState,_ =
                line.ToCharArray()
                |> Array.fold (fun (innerState,colNum) charNum ->
                    let value = charNum.ToString() |> int
                    // Add to map
                    (Map.add (rowNum,colNum) value innerState),colNum + 1
                ) (state,0)
            newState,rowNum + 1            
        ) (Map.empty,0)
    
module Part1 =
    open System.IO
    open Shared

    let solution inputFile =
    
        let lines =
            File.ReadAllLines inputFile
        
        // maxRow an maxCol
        let maxRow = Array.length lines - 1

        let maxCol =
            lines.[0].Length - 1

        let points,_ = getAllPointsMap lines

        let mins,_ = getMinimumPoints points maxRow maxCol

        List.sumBy (fun m -> m+1) mins

module Part2 = 
    open Shared
    open System.IO

    let getAdjacentPointsForBasin points maxRow maxCol row col =
        [(row - 1,col);
         (row, col + 1);
         (row + 1, col);
         (row,col - 1)
        ]
        |> List.filter (fun (x,y) ->
            // get the value of that point
            // it cannot be 9
            if (x <= maxRow) && (x>=0) && (y <= maxCol) && (y >= 0) then
                let pv = Map.find (x,y) points
                (pv <> 9)
            else
                false
        )
        |> Set.ofList

    let getAllAdjacentPointsForBasin points maxRow maxCol row col =

        let rec getAdjPoints points maxRow maxCol adjPoints newPoints =
            match Set.count newPoints with
            | 0 -> 
                adjPoints
            | _ ->
                let nxtPoints =
                    newPoints
                    |> Set.map (fun (x,y) ->
                        getAdjacentPointsForBasin points maxRow maxCol x y
                    )
                    |> Set.unionMany
                let newAdjPoints = Set.union adjPoints newPoints
                let netNewAdjPoints = Set.difference nxtPoints newAdjPoints
                getAdjPoints points maxRow maxCol newAdjPoints netNewAdjPoints

        let newAdjPoints =
            getAdjacentPointsForBasin points maxRow maxCol row col
        
        getAdjPoints points maxRow maxCol Set.empty newAdjPoints

    let solution inputFile = 
        
        let lines =
            File.ReadAllLines inputFile
        
        // maxRow an maxCol
        let maxRow = Array.length lines - 1

        let maxCol =
            lines.[0].Length - 1

        let points,_ = getAllPointsMap lines
        let _,minPoints = getMinimumPoints points maxRow maxCol

        // find the basin for each min point
        let basinForMinPoints = 
            minPoints
            |> List.map (fun (x,y) ->
                // get the basin points
                let s = getAllAdjacentPointsForBasin points maxRow maxCol x y
                Set.count s ,s
            )
        //printfn "basin:%A" basinForMinPoints
        basinForMinPoints
        |> List.unzip
        |> fst
        |> List.sortDescending
        |> List.take 3
        |> List.fold (fun state i ->
            state * i
        ) 1