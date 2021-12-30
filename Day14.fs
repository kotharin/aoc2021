namespace Day14

module Part1 = 
    open System.IO

    let substitute (current:string) subMap =
        // get the pairs from the current string
        let allPairs = current.ToCharArray() |> Array.pairwise

        // for each pair find the insertion and substitute
        let subPair (pairs:(char*char) array) map =
            // sub the first pair
            let f,p = Array.head pairs
            let fp = string f + string p
            let subString =
                Map.tryFind fp map
                |> Option.map (fun ins ->
                    fp.Insert(1,ins)
                )
                |> Option.defaultValue fp
            // Use the rest of the pairs to finish substitution
            Array.tail pairs
            |> Array.fold (fun (newS:string) (a,b) ->
                let pair = string a + string b
                // remove the last character from the string
                let s = newS.Remove(newS.Length - 1)
                Map.tryFind pair map
                |> Option.map(fun ins ->
                    s + (string a) + ins + (string b)
                )
                |> Option.defaultValue (s + pair)
            ) subString
        subPair allPairs subMap

    let parseSubstitutions lines =
        lines
        |> Array.fold (fun map (line:string) ->
            if (line.Trim().Length > 0) then
                let parts = line.Split([|'-';'>'|])
                Map.add (parts.[0].Trim()) (parts.[2].Trim()) map
            else
                map
        ) Map.empty
    let solution inputFile =
        let lines = File.ReadAllLines inputFile

        let startingString = Array.head lines

        let substitutions = parseSubstitutions (Array.tail lines)

        let sorted =
            ([0..9]
            |> Seq.fold (fun s _ ->
                substitute s substitutions
            ) startingString).ToCharArray()
            |> Array.countBy id
            |> Array.sortBy snd
        let (_,max),(_,min) = (Array.last sorted),(Array.head sorted)

        max-min
