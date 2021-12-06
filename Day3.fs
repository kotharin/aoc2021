namespace Day3

module Part1 = 
    open System
    open System.IO


    let solution inputFile = 

        let numbersArrayMap =
            File.ReadAllLines inputFile
            |> Seq.fold (fun (numbersMap) line ->
                    let ints = line.ToCharArray() |> Array.map (fun c -> (int)(c.ToString()))
                    let newArray =
                        Array.mapi (fun i inum ->
                            let iArray = Map.tryFind i numbersMap |> Option.defaultValue Array.empty
                            i,Array.append iArray [|inum|]
                        ) ints
                    Map.ofArray newArray
                ) (Map.empty)

        let countOfNumbers = Map.find 0 numbersArrayMap |> Array.length

        // get count of 1's to see if its majority or not
        let mostCommon,leastCommon =
            [|0..(Map.count numbersArrayMap) - 1|]
            |> Array.map (fun i -> Map.find i numbersArrayMap)
            |> Array.map (fun nums ->
                let numOf1 = Array.filter (fun n -> n = 1) nums |> Array.length
                if numOf1 >= countOfNumbers/2 then
                    1,0
                else
                    0,1
            )|> Array.unzip

        let gammaRate =
            mostCommon
            |> Array.fold (fun s i -> s + i.ToString()) "" 
            |> (fun r -> Convert.ToInt32(r,2))
        let epsilonRate =
            leastCommon
            |> Array.fold (fun s i -> s + i.ToString()) "" 
            |> (fun r -> Convert.ToInt32(r,2))

        gammaRate*epsilonRate