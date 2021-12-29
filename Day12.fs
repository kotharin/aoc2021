namespace Day12


module Part1 =
    open System.IO

    let parse (line:string) =
        let seg = line.Split([|'-'|])
        seg.[0],seg.[1]

    (*
    let dfs graph v1 v2 =
        let rec rdfs graph n1 n2 visited currenPath listOfPaths =
            if (Set.contains n1 visited) then

            let newV = Set.add n1 visited
            let newCP = currentPath + "-" + n1
            if (n1 = n2) then
    *)
    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let adjList =
            lines
            |> Array.fold(fun graph line ->
                let node,neighbor = parse line
                // ad to the nodes list of neighbors
                let cn = Map.tryFind node graph |> Option.defaultValue List.empty
                Map.add node (neighbor::cn) graph
            ) Map.empty
            |> Map.toList
        ()