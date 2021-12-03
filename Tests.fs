module Tests

open System
open Xunit
open Day1

[<Fact>]
let ``Day1 Part1`` () =
    let increases = Day1.Part1.solution "Day1.Part1.txt"
    Assert.Equal(increases,1581)

[<Fact>]
let ``Day1 Part2`` () =
    let increases = Day1.Part2.solution "Day1.Part1.txt"
    Assert.Equal(1618,increases)
