module Tests

open System
open Xunit
open System.Numerics

[<Fact>]
let ``Day1 Part1`` () =
    let increases = Day1.Part1.solution "Day1.Part1.txt"
    Assert.Equal(increases,1581)

[<Fact>]
let ``Day1 Part2`` () =
    let increases = Day1.Part2.solution "Day1.Part1.txt"
    Assert.Equal(1618,increases)

[<Fact>]
let ``Day2 Part1`` () =
    let answer = Day2.Part1.solution "Day2.Part1.txt"
    let expected = 1690020I
    let diff = if (BigInteger.Subtract(expected , answer) = 0I) then true else false
    Assert.True(diff)

[<Fact>]
let ``Day2 Part2`` () =
    let answer = Day2.Part2.solution "Day2.Part1.txt"
    let expected = 1408487760I
    let diff = if (BigInteger.Subtract(expected , answer) = 0I) then true else false
    Assert.True(diff)

[<Fact>]
let ``Day3 Part1`` () =
    let answer = Day3.Part1.solution "Day3.txt"
    let expected = 845186
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day3 Part2`` () =
    let answer = Day3.Part2.solution "Day3.txt"
    let expected = 4636702
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day4 Part1`` () =
    let answer = Day4.Part1.solution "Day4.txt"
    let expected = 11774//4512
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day4 Part2`` () =
    let answer = Day4.Part2.solution "Day4.txt"
    let expected = 4495//1924
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day5 Part1`` () =
    let answer = Day5.Part1.solution "Day5.txt"
    let expected = 6007//5
    Assert.Equal(expected,answer)

(*[<Fact>]
let ``Day5 Traversal Test`` () =
    let p1 = {Day5.Shared.Point.X = 9; Day5.Shared.Point.Y = 7}
    let p2 = {Day5.Shared.Point.X = 7; Day5.Shared.Point.Y = 9}

    let answer = Day5.Part2.coveredPoints p1 p2
    let expected = 3
    Assert.Equal(expected,(List.length answer))
*)
[<Fact>]
let ``Day5 Part2`` () =
    let answer = Day5.Part2.solution "Day5.txt"
    let expected = 19349//12
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day6 Part1`` () =
    let answer = Day6.Part1.solution "Day6.txt"
    let expected = 390011I//5934
    Assert.Equal(expected,answer)

(* Need to optimize this solution *)
(*
[<Fact>]
let ``Day6 Part2`` () =
    let answer = Day6.Part2.solution "Day6.txt"
    let expected = 390011I//5934
    Assert.Equal(expected,answer)
*)
[<Fact>]
let ``Day7 Part1`` () =
    let answer = Day7.Part1.solution "Day7.txt"
    let expected = 343441//37
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day7 Part2`` () =
    let answer = Day7.Part2.solution "Day7.txt"
    let expected = 98925151//168
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day8 Part1`` () =
    let answer = Day8.Part1.solution "Day8.txt"
    let expected = 449//26
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day8 Part2`` () =
    let answer = Day8.Part2.solution "Day8.txt"
    let expected = 968175//61229
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day9 Part1`` () =
    let answer = Day9.Part1.solution "Day9.txt"
    let expected = 560//15
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day9 Part2`` () =
    let answer = Day9.Part2.solution "Day9.txt"
    let expected = 959136//1134
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day10 Part1`` () =
    let answer = Day10.Part1.solution "Day10.txt"
    let expected = 268845//26397
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day10 Part2`` () =
    let answer = Day10.Part2.solution "Day10.txt"
    let expected = 4038824534I//288957
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day11 Part1`` () =
    let answer = Day11.Part1.solution "Day11.txt"
    let expected = 1655//1656
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day11 Part2`` () =
    let answer = Day11.Part2.solution "Day11.txt"
    let expected = 337//195
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day13 Part1`` () =
    let answer = Day13.Part1.solution "Day13.txt"
    let expected = 671//17
    Assert.Equal(expected,answer)

[<Fact>]
let ``Day13 Part2`` () =
    let answer = Day13.Part2.solution "Day13.txt"
    let expected = "PCPHARKL"
    Assert.Equal(expected,answer)