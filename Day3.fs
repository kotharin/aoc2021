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

module Part2 = 
    open System
    open System.IO

    let solution inputFile =
        let lines = File.ReadAllLines inputFile

        let getMostCommon nums =
            let numOf1,numOf0 = Array.partition (fun n -> n = 1) nums

            if (numOf1 >= numOf0) then
                1
            else
                0
        let getLeastCommon nums =
            let numOf1,numOf0 = Array.partition (fun n -> n = 1) nums

            if (numOf0 <= numOf1) then
                0
            else
                1

        let getValuesAtPosition (nums:string array) position =
            nums
            |> Array.map (fun num ->
                let numChars = num.ToCharArray()
                let charAt = Array.get numChars position
                (int)(charAt.ToString())
            )

        let hasValueAtPosition position value (num:string) =
            let numChars = num.ToCharArray() 
            (string)(Array.get numChars position) = value.ToString()

        // Filter down data based on the filter function
        let rec getFilteredValue filterFunction nums bitPosition =
            if (Array.length nums > 1) then
                // get the values at the current bit position
                let valuesAtPosition = getValuesAtPosition nums bitPosition
                // get the most common value in those values
                let mostCommonValue = filterFunction valuesAtPosition
                // Filter rows based on the most common value
                let newLines =
                    nums
                    |> Array.filter (fun num -> hasValueAtPosition bitPosition mostCommonValue num)
                getFilteredValue filterFunction newLines (bitPosition + 1)
            else
                nums

        // oxygen generator rating
        // Use the mostCommon function to filter down values
        let oxygenGenerator =
            getFilteredValue getMostCommon lines 0

        let oxygenGeneratorRating =
            Array.get oxygenGenerator 0
            |> (fun r -> Convert.ToInt32(r,2))

        // C02 generator rating
        // Use the leastCommon function to filter down values
        let co2Scrubber =
            getFilteredValue getLeastCommon lines 0

        let co2ScrubberRating =
            Array.get co2Scrubber 0
            |> (fun r -> Convert.ToInt32(r,2))

        oxygenGeneratorRating*co2ScrubberRating