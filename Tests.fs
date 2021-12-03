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


