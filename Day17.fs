namespace Day17

module Part1 =
    open System
    open System.IO
    (* 
        X and Y are completely independent
        The velocity shooting up, will be the same velocity
        it has when it crosses Y=0 on its way down.
        So, the next step it takes will be y-velocity + 1
        in the downward direction. That needs to be within 
        the bounds of the target

        V + 1 = Ymax
        V = Ymax - 1

        The max height it reaches is SUM (1..V) = (V+1)*V/2
    *)

    let solution inputFile =

        let line = File.ReadAllText inputFile

        // extract the max Y for the target area
        let ydata = line.Split([|'y';'='|]).[3]
        let highestY = ydata.Split([|'.';'.'|])

        let maxY = Math.Abs(Convert.ToInt32(highestY.[0]))

        (maxY*(maxY-1))/2

