namespace Day6

module Shared =

    let getNextGeneration generationCount currentGeneration =

        let rec nextGen genNumber currentGeneration nextGeneration newFishGeneration  =
            match currentGeneration with
            | currentFish::tail ->
                let newNextGen,newFishGen = 
                    if (currentFish = 0) then
                        List.append nextGeneration [6], List.append newFishGeneration [8]
                    else
                        List.append nextGeneration [currentFish - 1],newFishGeneration
                nextGen genNumber tail newNextGen newFishGen
            | [] ->
                List.append nextGeneration newFishGeneration
        
        [0..generationCount]
        |> List.fold (fun newGeneration generationNumber ->
            nextGen generationNumber newGeneration List.empty List.empty
        ) currentGeneration

module Part1 =
    open System
    open System.IO
    open Shared

    let solution inputFile =
        let line = File.ReadAllText inputFile
        let initGeneration =
            line.Split([|','|])
            |> Array.map (fun s -> int (s.Trim()))
            //|> Array.toList

        // split the inital population and run
        let genChunks = Array.chunkBySize 1 initGeneration

        let lastGeneration = 
            genChunks
            |> Array.map (fun initGen ->
                getNextGeneration 79 (initGen |> List.ofArray)
            )
            |> List.concat

        
        List.length lastGeneration

module Part2 =
    open System.IO
    open Shared


    let solution inputFile =

        let mapOfFishDays =
            [1..5]
            |> List.map(fun genDays ->
                let genList = getNextGeneration 255 [genDays]
                printfn "days:%i" genDays
                genDays, List.length genList
            )|> Map.ofList

        let line = File.ReadAllText inputFile
        let initGenerationGroups =
            line.Split([|','|])
            |> Array.map (fun s -> int (s.Trim()))
            |> Array.groupBy id
            |> Array.map (fun (startDay,finalGens) -> 
                startDay, Array.length finalGens
            )

        let totalCount =
            initGenerationGroups
            |> Array.fold (fun totalCount (startDay,multiplier) ->
                // Get the count of the final generation for this startDay
                let finalGenCount = Map.find startDay mapOfFishDays
                totalCount + bigint(finalGenCount * multiplier)
            ) 0I

        totalCount