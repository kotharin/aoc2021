namespace Day15

module Shared =
    open System

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


    //[<CustomEquality; CustomComparison >]
    type Node = {
        X:int
        Y:int
        Risk: int
        Cost: int
        Priority: int
        AdjacentNodes: (int*int) array
    } 
    with
        (*
        static member compare (n1:Node) (n2:Node) =
            //printfn "n1:%A" n1
            //printfn "n2:%A" n2
            let c = 
                if ((n1.X = n2.X) && (n1.Y = n2.Y)) then
                    0
                else
                    n1.Cost - n2.Cost

            //printfn "c:%i" c
            c

        override x.Equals(yobj) =  
            match yobj with 
            | :? Node as y -> 
                (x.X = y.X) && (x.Y = y.Y) && (x.Risk = y.Risk)
            | _ -> false

        override x.GetHashCode() =
            (x.X * 1000) + x.Y

        interface System.IComparable with
            member x.CompareTo yobj = 
                match yobj with 
                  | :? Node as y -> Node.compare x y
                  | _ -> invalidArg "yobj" "cannot compare value of different types" 
        *)
        static member parse maxX maxY curX curY (risk:string) =
            let an = getAdjacentLocations maxX maxY curX curY
            {
                X = curX
                Y = curY
                Node.Risk = (int)(risk.Trim())
                Cost = 64000
                Priority = 64000
                Node.AdjacentNodes = an
            }

    let heuristicCost (n1:Node) (n2:Node) =
        (n2.X - n1.X) + (n2.Y - n1.Y)

module Part1 =
    open Shared
    open System.IO

    let rec iterate destinationNode (openNodes:List<Node>) closed (nodesMap:Map<(int*int),Node>)  =
        // get the current node to work on
        let currentNode = List.minBy (fun n -> n.Cost) openNodes
        //printfn "cn:%A" currentNode
        let onPQ = List.filter (fun n -> not (n = currentNode)) openNodes
        // add to closed set
        let newClosed = Set.add (currentNode.X,currentNode.Y) closed
        //printfn "closed:%A" newClosed

        //printfn "nm:%A" nodesMap

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
                    //printfn "n:%A" n
                    let newCost = currentNode.Cost + n.Risk
                    if (((not (List.contains n newPQ)) && (not (Set.contains (n.X,n.Y) newClosed))) || (newCost < n.Cost)) then
                        let n2 = {n with Cost = newCost;Priority = newCost}
                        //printfn "n2 in set:%A" (Set.contains n2 newPQ)
                        //printfn "adding:%A" n2
                        let newSet = n2::newPQ
                        //printfn "newSet:%A" newSet
                        newSet,(Map.add (n2.X,n2.Y) n2 newNodesMap)
                    else
                        newPQ,newNodesMap
                ) (onPQ, nodesMap)
            //printfn "newPQ:%A" newOpenNodes
            //System.Console.ReadLine()
            iterate destinationNode newOpenNodes newClosed newNodesMap
        
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

        let updatedNodesMap = Map.add (0,0) {sn with Priority = 0; Cost = 0} nodesMap

        let openNodes = List.singleton (Map.find (0,0) updatedNodesMap)

        let destinationNode = Map.find (maxX,maxY) nodesMap

        let cost = iterate destinationNode openNodes Set.empty updatedNodesMap

        cost