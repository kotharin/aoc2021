namespace Day4

module Shared =

    type State = Undrawn | Drawn

    type Position = {
        X: int
        Y: int
    }

    type Board = {
        AllNumbers: Map<int,Position>
        DrawnNumbers: Set<int*Position>
        UndrawnNumbers: Set<int*Position>
        Rows: int
    } with
        static member empty =
            {
                AllNumbers = Map.empty
                DrawnNumbers = Set.empty
                UndrawnNumbers = Set.empty
                Rows = 0
            }
        static member addRow row (board:Board) =
            // get the next rowNumber
            let rowNumber = board.Rows

            row
            |> Array.fold ( fun (newBoard,colNum) colVal ->
                let position = {Position.X=rowNumber; Position.Y = colNum}
                let allNums = Map.add colVal position newBoard.AllNumbers
                let undrawnNums = Set.add (colVal,position) newBoard.UndrawnNumbers
                ({newBoard with AllNumbers = allNums; UndrawnNumbers = undrawnNums;Rows=rowNumber + 1},colNum + 1)
            ) (board,0)
            |> fst

        static member hasOneRowOrColumnFilled board =
            if (board.DrawnNumbers.Count >= 5) then
                let positions = 
                    Set.toArray board.DrawnNumbers
                    |> Array.unzip
                    |> snd
                let rowGroups =
                    positions
                    |> Array.groupBy (fun p -> p.X)

                let columnGroups =
                    positions
                    |> Array.groupBy (fun p -> p.Y)

                (Array.exists (fun (_, posns) -> Array.length posns >= 5 ) rowGroups) || (Array.exists (fun (_, posns) -> Array.length posns >= 5 ) columnGroups)
            else
                false
        static member applyDrawnNumberToBoard number board =
            // check if the number exists on theboard
            // if so add it to the drawnNumbers set
            Map.tryFind number board.AllNumbers
            |> Option.map( fun position ->
                // Add to drawn numbers list
                let newDrawNumbers = Set.add (number,position) board.DrawnNumbers
                // remove form undrawn numbers
                let newUndrawnNumbers = Set.remove (number,position) board.UndrawnNumbers
                
                {board with DrawnNumbers = newDrawNumbers; UndrawnNumbers = newUndrawnNumbers}
            )
            |> Option.defaultValue board

        static member print board =
            let boardByPosition =
                board.AllNumbers
                |> Map.toArray
                |> Array.map (fun (v,p) -> p,v)
                |> Map.ofArray

            [0..4]
            |> Seq.iter (fun row ->
                [0..4]
                |> Seq.iter (fun col -> 
                    let position = {Position.X=row;Position.Y=col}
                    let value = (Map.find position boardByPosition)
                    if (col = 4) then
                        printfn "%i" value
                    else
                        printf "%i " value
                )
            )

            printfn "Drawn Numbers:%A" board.DrawnNumbers


    let parseDrawNumbers (line:string) =
        line.Split([|','|])
        |> Array.map int
    
    let parseBoardRowData (line:string) =
        line.Trim().Split([|' '|]) 
        |> Array.filter (fun s -> s.Trim().Length > 0)
        |> Array.map (fun v -> 
            int(v.Trim()))

    let parseBoards allBoards =
        let rec parseBoardData (listBoardData:List<string>) listBoards currentBoard =
            match listBoardData with
            | boardRow::tail ->
                if (boardRow.Trim().Length = 0) then
                    // Add previous board to the list
                    if (Option.isSome currentBoard) then
                        // Add to the list
                        let newBoardList = List.append listBoards [currentBoard] 
                        parseBoardData tail newBoardList (Some Board.empty)
                    else
                        // most likely first board
                        parseBoardData tail listBoards (Some Board.empty)
                else
                    // accumulate new board data
                    let boardNewRow = parseBoardRowData boardRow
                    // create board if it doesn't exist
                    let board = currentBoard |> Option.defaultValue (Board.empty)
                    parseBoardData tail listBoards (Some (Board.addRow boardNewRow board))
            | [] ->
                List.append listBoards [currentBoard]
        parseBoardData (allBoards |> List.ofArray) List.empty None

module Part1 =

    open Shared
    open System.IO

    let solution inputFile =

        let rec playGame drawNumbers boards = 
            match drawNumbers with
            | nextNum::rest ->
                // apply the next number across the boards
                // and filter to any boards that might have
                // a complet row or column
                let newBoards =
                    boards
                    |> List.map (fun board -> Board.applyDrawnNumberToBoard nextNum board)

                let completeBoards =
                    newBoards
                    |> List.filter (Board.hasOneRowOrColumnFilled)

                if (List.length completeBoards) > 0 then
                    Some (List.head completeBoards), nextNum
                else
                    playGame rest newBoards
            | [] ->
                None,0

        let drawNums, allBoards = 
            File.ReadAllLines inputFile
            |> Array.splitAt 1

        let drawNumbers = parseDrawNumbers drawNums.[0] |> List.ofArray

        let bingoBoards =
            parseBoards allBoards
            |> List.choose id

        let winningBoard, lastNum =
            playGame drawNumbers bingoBoards

        winningBoard
        |> Option.map(fun wb ->
            let sumOfUnmarked =
                wb.UndrawnNumbers
                |> Set.toSeq
                |> Seq.sumBy fst
            lastNum * sumOfUnmarked
        )
        |> Option.defaultValue -1

module Part2 = 
    open System.IO
    open Shared

    let solution inputFile =

        let rec playGame drawNumbers boards =

            match drawNumbers with
            | currentNumber::tailNumbers ->
                // Apply number to all boards
                let newBoards = 
                    boards
                    |> Set.map (fun board -> Board.applyDrawnNumberToBoard currentNumber board)

                // Identify any boards that have been "completed"
                let uncompletedBoards =
                    newBoards
                    |> Set.filter Board.hasOneRowOrColumnFilled
                    |> Set.difference newBoards

                if (Set.count uncompletedBoards > 0) then
                    // If there is atleast one board still incomplete, play on
                    playGame tailNumbers uncompletedBoards
                else
                    // Last board was completed
                    newBoards |> Set.toSeq |> Seq.head |> Some, currentNumber
            | [] ->
                None, -1
        let drawNums, allBoards = 
            File.ReadAllLines inputFile
            |> Array.splitAt 1

        let drawNumbers = parseDrawNumbers drawNums.[0] |> List.ofArray

        let bingoBoards =
            parseBoards allBoards
            |> List.choose id
            |> Set.ofList

        let lastBoard, lastNum = playGame drawNumbers bingoBoards

        lastBoard
        |> Option.map (fun wb ->
            let sumOfUnmarked =
                wb.UndrawnNumbers
                |> Set.toSeq
                |> Seq.sumBy fst            
            lastNum * sumOfUnmarked
        )
        |> Option.defaultValue -1
        
        