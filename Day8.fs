namespace Day8

module Part1 = 
    open System
    open System.IO

    let solution inputFile =

        let lines = 
            File.ReadAllLines inputFile
        
        let _,outputs =
            lines
            |> Array.map (fun line ->
                let sections = line.Trim().Split([|'|'|])
                sections.[0], sections.[1]
            )
            |> Array.unzip
        
        outputs
        |> Array.map (fun output ->
            output.Trim().Split([|' '|])
            |> Array.filter(fun digit -> 
                let dl = digit.Trim().Length
                (dl = 2) || (dl = 4) || (dl = 3) || (dl = 7))
            |> Array.length
        )
        |> Array.sum
        
module Part2 =
    open System
    open System.IO

    // Get 1,4,7,8 since they have fixed lengths
    let separateKnownFromUnknownNumbers (outputs:string array) =

        let setOfUnknowns = outputs |> Set.ofArray

        [|2,1;4,4;3,7;7,8|]
        |> Array.fold (fun (knownMap,unknowns) (count,num) ->
            let kn =
                outputs
                |> Array.filter(fun digit -> 
                    digit.Trim().Length = count
                )
                |> Array.head
            // Add the known number to the Map
            let map = Map.add num (Set.ofList [kn]) knownMap
            let us = Set.remove kn unknowns
            map,us
        ) (Map.empty, setOfUnknowns)

    (*
    let get6 outputs knownNumbers =
        
        // Get all 6 letter combinations
        // 0,6,9
        let combos =
            outputs
            |> Array.filter (fun o -> 
                o.Trim().Length = 6
            )
        combos
    *)
    let solution inputFile =

        let outputs = 
            File.ReadAllLines inputFile
            |> Array.map (fun line ->
                let sections = line.Trim().Split([|'|'|])
                sections.[0], sections.[1]
            )
            |> Array.unzip
            |> snd
        
        let knownNumbersMap = separateKnownFromUnknownNumbers outputs

        ()