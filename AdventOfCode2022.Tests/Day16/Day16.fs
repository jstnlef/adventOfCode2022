module Tests.Day16

open Day16
open Xunit

[<Theory>]
[<InlineData("Day16/sample.txt", 1651)>]
[<InlineData("Day16/sample2.txt", 210)>]
[<InlineData("Day16/input.txt", 1873)>]
let ``Most pressure you can release in 30 minutes`` (fileName: string, expected: int) =
  let result = Pipes.parse fileName |> Pipes.findMostPressureYouCanRelease 30
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day16/sample.txt", 1707)>]
[<InlineData("Day16/input.txt", 2425)>]
let ``Most pressure you and an elephant can release in 30 minutes`` (fileName: string, expected: int) =
  let result =
    Pipes.parse fileName |> Pipes.findMostPressureYouAndAnElephantCanRelease 26

  Assert.Equal(expected, result)
