namespace Day15

module Shared =

    // Get valid adjacent locations
    let getAdjacentLocations maxX maxY x y =
        [|
            (x - 1,y);
            (x+1,y);
            (x,y-1);
            (x,y+1)
        |]
        |> Array.filter (fun (curX, curY) ->
            ((0<=curX) && (0<=curY) && (curX <= maxX) && (curY <= maxY))
        )

    type Node = {
        X:int
        Y:int
        Risk: int
        Cost: int
        AdjacentNodes: (int*int) array
    } 
    with
        static member parse maxX maxY curX curY (risk:string) =
            let an = getAdjacentLocations maxX maxY curX curY
            {
                X = curX
                Y = curY
                Node.Risk = (int)(risk.Trim())
                Cost = 64000
                Node.AdjacentNodes = an
            }

    let rec cheapestPath destinationNode (openNodes:List<Node>) closed (nodesMap:Map<(int*int),Node>)  =
        // get the current node to work on
        let currentNode = List.minBy (fun n -> n.Cost) openNodes

        let onPQ = List.filter (fun n -> not (n = currentNode)) openNodes
        // add to closed set
        let newClosed = Set.add (currentNode.X,currentNode.Y) closed

        if ((currentNode.X = destinationNode.X) && (currentNode.Y = destinationNode.Y)) then
            currentNode.Cost
        else
            let neighbors = 
                currentNode.AdjacentNodes
            // check cost and add to PQ if appropriate
            let newOpenNodes,newNodesMap =
                neighbors
                |> Array.fold (fun (newPQ, newNodesMap) (nx,ny) ->
                    let n = Map.find (nx,ny) newNodesMap
                    let newCost = currentNode.Cost + n.Risk
                    if (((not (List.contains n newPQ)) && (not (Set.contains (n.X,n.Y) newClosed))) || (newCost < n.Cost)) then
                        let n2 = {n with Cost = newCost}
                        let newSet = n2::newPQ
                        newSet,(Map.add (n2.X,n2.Y) n2 newNodesMap)
                    else
                        newPQ,newNodesMap
                ) (onPQ, nodesMap)
            cheapestPath destinationNode newOpenNodes newClosed newNodesMap

module Part1 =
    open Shared
    open System.IO

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let maxX = lines.[0].Length - 1
        let maxY = Array.length lines - 1

        let _, nodesMap = 
            lines
            |> Array.fold ( fun (x,nodes) line ->

                let _,rowMap  =
                    // Go through entire line and create nodes
                    line.ToCharArray()
                    |> Array.fold (fun (y,nodeMap) cv ->
                        let node = Node.parse maxX maxY x y (cv.ToString())
                        y+1, Map.add (x,y) node nodeMap
                    ) (0,nodes)
                x+1, rowMap
            ) (0,Map.empty)

        // set start node priority and cost to 0
        let sn = Map.find (0,0) nodesMap

        let updatedNodesMap = Map.add (0,0) {sn with Cost = 0} nodesMap

        let openNodes = List.singleton (Map.find (0,0) updatedNodesMap)

        let destinationNode = Map.find (maxX,maxY) nodesMap

        let cost = cheapestPath destinationNode openNodes Set.empty updatedNodesMap

        cost

module Part2 = 
    open Shared
    open System.IO

    let scaleoutValueAcross  scaleX scaleY maxX maxY  origNode nodeMap =
        // scale out five times
        [|1..4|]
        |> Array.fold (fun map multiplier ->
            let yOffset1 = (scaleY + 1) * multiplier
            let newY1 =  origNode.Y + yOffset1
            let r = (origNode.Risk + multiplier) % 9
            let newRisk = if (r = 0) then 9 else r
            let adjacentNodes = getAdjacentLocations maxX maxY origNode.X newY1
            let newNode1 = {origNode with Y = newY1; X=origNode.X; Risk = newRisk; AdjacentNodes = adjacentNodes}

            Map.add (origNode.X, newY1) newNode1 map

        ) nodeMap

    let scaleoutValueExtraCol  scaleX scaleY maxX maxY origNode nodeMap =
        [|0..4|]
        |> Array.fold (fun map colMultiplier ->
            [|1..4|]
            |> Array.fold (fun subMap rowMultiplier ->
                let xOffset = (scaleX + 1) * rowMultiplier
                let yOffset = (scaleY + 1) * colMultiplier
                let newX = origNode.X + xOffset
                let newY = origNode.Y + yOffset
                let r = (origNode.Risk + rowMultiplier + colMultiplier) % 9
                let newRisk = if (r = 0) then 9 else r
                let adjacentNodes = getAdjacentLocations maxX maxY newX newY
                let newNode = {origNode with X = newX;Y=newY;Risk = newRisk;AdjacentNodes=adjacentNodes}
                Map.add (newX,newY) newNode subMap
            ) map
        ) nodeMap

    let print maxX maxY (nodeMap:Map<(int*int),Node>) =
        printfn "grid:"
        [|0..maxX|]
        |> Array.iter (fun row ->
            let rd = 
                [|0..maxY|]
                |> Array.fold (fun rowData col ->
                    let node = Map.find (row,col) nodeMap
                    rowData + node.Risk.ToString()
                ) ""
            printfn "%s" rd
        )

    let solution inputFile =
        let lines = File.ReadAllLines inputFile

        let scaleX = lines.[0].Length - 1
        let scaleY = Array.length lines - 1

        let largeMaxX = ((scaleX + 1) * 5) - 1
        let largeMaxY = ((scaleY + 1) * 5) - 1

        let _, nodesMap = 
            lines
            |> Array.fold ( fun (x,nodes) line ->

                let colCount,rowMap  =
                    // Go through entire line and create nodes
                    line.ToCharArray()
                    |> Array.fold (fun (y,nodeMap) cv ->
                        let node = Node.parse largeMaxX largeMaxY x y (cv.ToString())
                        let newNodeMap =
                            scaleoutValueAcross scaleX scaleY largeMaxX largeMaxY node nodeMap
                            |> scaleoutValueExtraCol scaleX scaleY largeMaxX largeMaxY node
                            |> Map.add (x,y) node 
                        y+1, newNodeMap
                    ) (0,nodes)
                x+1, rowMap
            ) (0,Map.empty)

        // set start node priority and cost to 0
        let sn = Map.find (0,0) nodesMap

        let updatedNodesMap = Map.add (0,0) {sn with Cost = 0} nodesMap

        let openNodes = List.singleton (Map.find (0,0) updatedNodesMap)

        let destinationNode = Map.find (largeMaxX,largeMaxY) nodesMap

        let cost = cheapestPath destinationNode openNodes Set.empty updatedNodesMap

        cost
