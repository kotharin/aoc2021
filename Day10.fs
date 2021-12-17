namespace Day10

module Shared =
    open System

    type CharStack = CharData of char list

    type CharStack with
        static member pop (CharData stack) =
            match stack with
            | head::tail ->
                head,tail
            | [] ->
                failwith "Stack empty"

        static member push data (CharData stack) =
            CharData (data::stack)

        static member empty =
            CharData List.empty

    let isOpenChar c =
        (c = '(') || (c='{') || (c='[') || (c='<')

module Part1 =
    open System.IO
    open Shared


    let getFirstCorruptChar openCloseMap  (line:string) =
        // loop over the characters in the line

        let lineChars = line.ToCharArray()

        let corruptChars,_ =
            lineChars
            |> Array.fold (fun (cc, closeChars) lc ->
                // try to get the matching closing character
                if (isOpenChar lc) then
                    // get closing char and add to stack
                    let closingChar = Map.find lc openCloseMap
                    cc,(CharStack.push closingChar closeChars)
                else
                    // check if the closing char is the expected one
                    let ncc,newCloseChars = CharStack.pop closeChars
                    if (lc = ncc) then
                        // not corrupt
                        cc, (CharData newCloseChars)
                    else
                        // add to the list of corrupt chars
                        (lc::cc),(CharData newCloseChars)
            ) (List.empty, CharStack.empty)

        List.tryHead corruptChars

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let openCloseMap = 
            [('{','}');('(',')');('[',']');('<','>')]
            |> Map.ofList

        File.ReadAllLines inputFile
        |> Array.map (fun line ->
            getFirstCorruptChar openCloseMap line
        )
        |> Array.choose id
        |> Array.map (fun c ->
            match c with
            | ')' -> 3
            | ']' -> 57
            | '}' -> 1197
            | _ -> 25137
        )
        |> Array.sum
        
module Part2 =
    open Shared
    open System.IO

    let getIncompleteLineMissingChars openCloseMap  (line:string) =
        // loop over the characters in the line

        let lineChars = line.ToCharArray() |> List.ofArray

        let rec getMissingClosingChars isCorrupt chars missingChars =

            match isCorrupt with
            | true ->
                // ignore corrupt lines
                None
            | false ->
                match chars with
                | [] ->
                    Some missingChars
                | lc::tail ->
                    // try to get the matching closing character
                    if (isOpenChar lc) then
                        // get closing char and add to stack
                        let closingChar = Map.find lc openCloseMap
                        getMissingClosingChars false tail (CharStack.push closingChar missingChars)
                    else
                        // check if the closing char is the expected one
                        let ncc,newMissingChars = CharStack.pop missingChars
                        if (lc = ncc) then
                            // not corrupt
                            getMissingClosingChars false tail (CharData newMissingChars)
                        else
                            // add to the list of corrupt chars
                            getMissingClosingChars true [] (CharStack.empty)

        getMissingClosingChars false lineChars CharStack.empty

    let openCloseMap = 
        [('{','}');('(',')');('[',']');('<','>')]
        |> Map.ofList

    let score chars =
        chars
        |> List.fold (fun  state c ->
            let newState = state * 5I
            let charPoints =
                match c with
                | ')' -> 1I
                | ']' -> 2I
                | '}' -> 3I
                | _ -> 4I
            newState + charPoints
        ) 0I
    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let incompletes = 
            lines
            |> Array.map (fun line ->
                getIncompleteLineMissingChars openCloseMap line
            )
            |> Array.choose id

        let median = (Array.length incompletes) / 2

        let costs =
            incompletes
            |> Array.map (fun (CharData cd) ->
                score cd
            )
            |> Array.sort
            
        costs.[median]