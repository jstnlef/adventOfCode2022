module Tests.Day16

open Day16
open Xunit

[<Theory>]
[<InlineData("Day16/sample.txt", 1651)>]
[<InlineData("Day16/input.txt", -1)>]
let ``What is the most pressure you can release in 30 minutes?`` (fileName: string, expected: int) =
  let result = Pipes.parse fileName |> Pipes.findMostPressureReleased 30
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day16/sample.txt", -1)>]
[<InlineData("Day16/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)
