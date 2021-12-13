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
    let separateKnownFromUnknownNumbers (inputs:string array) =

        let setOfUnknowns =
            inputs 
            |> Array.map (fun s -> Set.ofArray (s.ToCharArray()))
            |> Set.ofArray

        [|2,1;4,4;3,7;7,8|]
        |> Array.fold (fun (knownMap,unknowns) (count,num) ->
            let kn =
                inputs
                |> Array.filter(fun digit -> 
                    digit.Trim().Length = count
                )
                |> Array.head
            // Add the known number to the Map
            let map = Map.add num (Set.ofArray (kn.ToCharArray())) knownMap
            let us = Set.remove (Set.ofArray (kn.ToCharArray())) unknowns
            map,us
        ) (Map.empty, setOfUnknowns)

    
    let get6And9 (knownNumbersMap: Map<int,Set<char>>) (unknownNumbers: Set<Set<char>>) =
        
        // Get all 6 letter combinations
        // 0,6,9
        let combos =
            unknownNumbers
            |> Set.filter (fun o -> 
                Set.count o = 6
            )

        // Diff of 8 and 6 should be a member of 1
        let eight = Map.find 8 knownNumbersMap
        let one = Map.find 1 knownNumbersMap
        let four = Map.find 4 knownNumbersMap

        // Get 6
        let six =
            combos
            |> Set.filter (fun digit ->
                let diff = Set.difference eight digit
                Set.isProperSubset diff one
            )
            |> Set.minElement

        // remove from combos
        let combos =
            Set.remove six combos

        // get 9
        let nine =
            combos
            |> Set.filter (fun digit ->
                Set.isProperSubset four digit
            )|> Set.minElement

        let zero =
            Set.remove nine combos
            |> Set.minElement

        // Add to known  and remove from unknown
        let newKnown =
            Map.add 6 six knownNumbersMap
            |> Map.add 9 nine
            |> Map.add 0 zero

        let newUnknown =
            Set.remove nine unknownNumbers 
            |> Set.remove six
            |> Set.remove zero 

        newKnown, newUnknown

    let get23And5 knownNumbersMap unknownNumbers =
        let six = Map.find 6 knownNumbersMap
        let one = Map.find 1 knownNumbersMap
        let combos =
            unknownNumbers
            |> Set.filter (fun o -> 
                Set.count o = 5
            )

        // find 5, wihch has a diff of just 1 with 6
        let five = 
            combos
            |> Set.filter (fun digit ->
                Set.difference six digit
                |> Set.count =  1
            )
            |> Set.minElement
        
        let three =
            combos
            |> Set.filter (fun digit ->
                Set.isProperSubset one digit
            )
            |> Set.minElement

        // what remains should be 2
        let two =
            Set.remove five combos
            |> Set.remove three
            |> Set.minElement

        let newKnown =
            Map.add 2 two knownNumbersMap
            |> Map.add 3 three
            |> Map.add 5 five

        newKnown

    let getNumbers (inputs:string array) =
        let kN1,uN1 = separateKnownFromUnknownNumbers inputs
        let kN2,uN2 = get6And9 kN1 uN1
        let kN3 = get23And5 kN2 uN2

        kN3

    let getOutputNumbers knownNumber (outputs:string array) =

        // flip the known Numbers map
        let charsToNumberMap =
            knownNumber
            |> Map.fold (fun state num chars ->
                Map.add chars num state
            ) Map.empty
        
        outputs
        |> Array.map (fun s ->
            // find the equivalent number
            // convert to a char set
            let charSet =
                s.ToCharArray() |> Set.ofArray
            Map.find charSet charsToNumberMap
        )

    let solution inputFile =
            
        let allOutputNumbers =

            let inputs,outputs = 
                File.ReadAllLines inputFile
                |> Array.map (fun line ->
                    let sections = line.Trim().Split([|'|'|])
                    sections.[0], sections.[1]
                )
                |> Array.unzip
                
            Array.fold2 (fun state (input:string) (output:string) ->
                // separate the inputs and outputs
                let ins = input.Trim().Split([|' '|])
                let outs = output.Trim().Split([|' '|])

                // decipher the numbers from the inputs
                let knownNumbers = getNumbers ins

                let displayNumber =
                    getOutputNumbers knownNumbers outs
                    |>
                    Array.fold (fun s n ->
                        s + sprintf "%i" n
                    ) ""
                    |> int
                                        
                Array.append state [|displayNumber|]
            ) Array.empty inputs outputs

        Array.sum allOutputNumbers