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

        let mapOfFishDays =
            [1..5]
            |> List.map(fun genDays ->
                let genList = getNextGeneration 79 [genDays]
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
module Part2 =
    open System.IO
    open Shared


    let solution inputFile =

        let mapOfFishDays =
            [1..5]
            |> List.map(fun genDays ->
                let genList = getNextGeneration 255 [genDays]
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