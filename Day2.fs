namespace Day2

module Part1 =
    open System.IO
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
        printfn "d:%O,h:%O,d*h:%O" finalD finalH (BigInteger.Multiply(finalD,finalH))
        BigInteger.Multiply(finalD,finalH)