namespace Day1
open System
open System.IO


module Shared =
    type Comparison = Increase | Deecrease | Equal

    let compare x y =
        let diff = y - x
        match diff with
        | a when a < 0 -> Deecrease
        | b when b > 0 -> Increase
        | _ -> Equal

module Part1 =
    open Shared

    let solution inputFile =
        File.ReadAllLines inputFile
        |> Seq.pairwise
        |> Seq.filter (fun (x,y) -> 
            compare (int x) (int y) = Increase    
        )
        |> Seq.length

module Part2 =
    open Shared

    let sum3FromIndex (items:string array) index =
        (int)(Array.get items index) + (int)(Array.get items (index+1)) + (int)(Array.get items (index+2))

    let solution inputFile =
        
        let lines = File.ReadAllLines inputFile
        
        let sumNext3 = sum3FromIndex lines

        let increases =
            [0..lines.Length - 3]
            |> Seq.map (fun i ->
                sumNext3 i 
                )
            |> Seq.pairwise
            |> Seq.filter (fun (x,y) -> 
                compare (int x) (int y) = Increase    
            )
            |> Seq.length
        
        increases
