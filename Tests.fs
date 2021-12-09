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
    let expected = 5//1924
    Assert.Equal(expected,answer)