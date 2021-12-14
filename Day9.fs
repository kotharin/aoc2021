namespace Day9

module Part1 =
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
        |> List.fold (fun state row ->
            // get all the columns
            [0..maxCol]
            |> List.fold (fun innerState col ->
                // get list of adjacent points
                let adjacent = getAdjacentPoints maxRow maxCol row col
                // check if this is the minimum point compared to the adjacent points
                if (isSmallestComparedToAdjacent points adjacent row col) then
                    let minVal = Map.find (row,col) points
                    minVal::innerState
                else
                    innerState
            )state
        ) List.empty

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
    
    let solution inputFile =
    
        let lines =
            File.ReadAllLines inputFile
        
        // maxRow an maxCol
        let maxRow = Array.length lines - 1

        let maxCol =
            lines.[0].Length - 1

        let points,_ = getAllPointsMap lines

        let mins = getMinimumPoints points maxRow maxCol

        List.sumBy (fun m -> m+1) mins