namespace Day5

module Shared =
    open System

    type Point = {
        X: int
        Y: int
    } with
        static member parse (p: string) =
            let xy = p.Trim().Split([|','|])
            {
                X=int (xy.[0].Trim())
                Y=int (xy.[1].Trim())
            }

    type Range = {
        P1:Point
        P2:Point
    } with
        static member parse (r:string)=
            let pd = 
                r.Trim().Split([|'-';'>'|])
                |> Array.filter (fun x -> x.Trim().Length > 0)
            {
                P1 = Point.parse pd.[0]
                P2 = Point.parse pd.[1]
            }
        static member validRange range =
            (range.P1.X = range.P2.X) || (range.P1.Y = range.P2.Y)

        static member coveredPoints p1 p2 =
            if (p1.X = p2.X) then
                // it varies in Y
                let incr = -(p1.Y - p2.Y)/(Math.Abs(p1.Y - p2.Y))
                [p1.Y..incr..p2.Y]
                |> List.map (fun y -> 
                    {
                        Point.X = p1.X
                        Point.Y = y
                    }
                )
            else
                // it varies in X
                let incr = -(p1.X - p2.X)/(Math.Abs(p1.X - p2.X))
                [p1.X..incr..p2.X]
                |> List.map (fun x ->
                    {
                        Point.X = x
                        Point.Y = p1.Y
                    }
                )
                

module Part1 =
    open System.IO
    open Shared

    let solution inputFile =

        let lines = File.ReadAllLines inputFile

        let ranges = 
            lines
            |> Array.map (Range.parse)
            |> Array.filter Range.validRange
        
        let rangeCoveredPoints =
            ranges
            |> Array.fold (fun coveredPoints range ->
                let currentCP = Range.coveredPoints range.P1 range.P2
                let newCoveredPoints = 
                    currentCP
                    |> List.fold (fun cp point ->
                        let icp =
                            Map.tryFind point cp
                            |> Option.map (fun count ->
                                Map.add point (count+1) cp
                            ) |> Option.defaultValue (Map.add point 1 cp)
                        icp
                    ) coveredPoints
                newCoveredPoints
            ) Map.empty

        rangeCoveredPoints
        |> Map.filter (fun k t -> t>=2)
        |> Map.count
