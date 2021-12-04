namespace Day2

module Shared =
    open System.Numerics

    type Direction = 
        | Forward of bigint
        | Up of bigint
        | Down of bigint
    with
        static member fromString (s:string) =
            let parts = s.Split([|' '|])
            let mag = BigInteger.Parse parts.[1]
            match parts.[0] with
            | "forward" -> Forward(mag)
            | "up" -> Up(mag)
            | _ -> Down(mag)

module Part1 =
    open System.IO
    open Shared
    open System.Numerics

    let solution inputFile =
        let finalD, finalH = 
            File.ReadAllLines inputFile
            |> Seq.map Direction.fromString
            |> Seq.fold (fun (depth,horizontal) direction ->
                match direction with
                | Forward a -> depth, horizontal + a
                | Up a -> depth - a, horizontal
                | Down a -> depth + a, horizontal

            ) (0I,0I)
        BigInteger.Multiply(finalD,finalH)

module Part2 =
    open System.IO
    open Shared
    open System.Numerics

    let solution inputFile =
        let finalD, finalH, finalA = 
            File.ReadAllLines inputFile
            |> Seq.map Direction.fromString
            |> Seq.fold (fun (depth,horizontal,aim) direction ->
                match direction with
                | Forward a -> depth + (a*aim), horizontal + a, aim
                | Up a -> depth , horizontal, aim - a
                | Down a -> depth, horizontal, aim + a

            ) (0I,0I,0I)
        BigInteger.Multiply(finalD,finalH)
