namespace Day11

module Part1 =
    open System.IO

    let print maxRow maxCol grid =
        [0..maxRow]
        |> Seq.iter (fun row ->
            let line =
                [0..maxCol]
                |> Seq.fold (fun s col ->
                    sprintf "%s,%i" s (Map.find (row,col) grid)
                ) ""
            printfn "%s" line
        )

    // Get cells adjacent to the current cell (row,col)
    let getAdjacent maxRow maxCol row col =
        [|
            (row,col - 1);
            (row,col + 1);
            (row - 1,col);
            (row + 1,col);
            (row - 1,col - 1);
            (row + 1,col - 1);            
            (row + 1,col + 1);
            (row - 1,col + 1);
        |]
        |> Array.filter (fun (x,y) ->
            (x >= 0) && (x <= maxRow) && (y >= 0) && (y <= maxCol)
        )
    let incrementAllValues currentGrid =
        currentGrid
        |> Map.fold (fun (ng,nz) (r,c) v ->
            let newVal =
                if (v = 9) then
                    0
                else
                    v + 1
            let uz =
                if (newVal = 0) then
                    List.append nz [(r,c)]
                else nz
            let ug = Map.add (r,c) newVal ng
            ug,uz
        )(currentGrid,List.empty)

    let incrementCellValues grid cells =
        cells
        |> Seq.fold (fun (ng,nz) (r,c) ->
            let cv = Map.find (r,c) ng
            let newVal, nzRC =
                if (cv = 9) then
                    0, [(r,c)]
                elif ((cv = -1) || (cv = 0)) then
                    cv,List.empty
                else
                    cv + 1,List.empty
            let newZeroes = List.append nz nzRC
            Map.add (r,c) newVal ng,newZeroes
        ) (grid,List.empty)
    // See which cell is a 0
    // that cell needs to "flash"
    // and change th adjacent cell values
    let applyZero maxRow maxCol zeroes (grid:(Map<(int*int),int>)) =
        let rec apply zeroes grid =
            match zeroes with
            | [] ->
                grid,zeroes
            | (r,c)::tail ->
                // Loop through all the existing 0's and update adjacent cells
                let newGrid,newZeroes =
                    // Get the adjacent cells
                    let aCells = getAdjacent maxRow maxCol r c
                    // increment the adjacent cell values
                    let newG, newZ = incrementCellValues grid aCells
                    // add the list of new zeroes to the list of zeroes
                    let updatedZ = List.append tail newZ
                    // set the 0 that we just applied, to -1
                    let updatedG = Map.add (r,c) -1 newG
                    //printfn "applied 0 for %i,%i" r c
                    //print maxRow maxCol updatedG
                    updatedG, updatedZ
                apply newZeroes newGrid
        apply zeroes grid

    let countFlashesAndResetZeros grid =
        grid
        |> Map.fold (fun (g,zCount) (r,c) v ->
            let ng,count =
                if (v = -1) then
                    (Map.add (r,c) 0 g),zCount + 1
                else
                    g,zCount
            ng,count
        ) (grid,0)

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let maxRows = Array.length lines - 1

        let maxCols = lines.[0].Length - 1

        let grid,_ =
            lines
            |> Array.fold (fun (s1,row) line ->
                let values = line.ToCharArray()
                let newMap,_ =
                    values
                    |> Array.fold (fun (s2,col) cv ->
                        Map.add (row,col) (int(cv.ToString())) s2, col + 1
                    ) (s1,0)
                newMap,row + 1
            ) (Map.empty,0)

        let _,flashCount =
            [0..99]
            |> Seq.fold (fun (cg,flashCount) i ->
                // increment values
                let ng,nz = incrementAllValues cg
                //printfn "nz:%A" nz
                // apply the 0's (flashes)
                let ug, _ = applyZero maxRows maxCols nz ng
                // count the number of -1 (flashes) and reset to 0
                let newGrid,fc = countFlashesAndResetZeros ug
                //printfn "----------------"
                //print maxRows maxCols ug
                newGrid,(flashCount + fc)
            ) (grid,0)
        flashCount