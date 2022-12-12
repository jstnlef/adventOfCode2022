module Tests.Day12

open Day12
open Xunit

[<Theory>]
[<InlineData("Day12/sample.txt", 31)>]
[<InlineData("Day12/input.txt", -1)>]
let ``Fewest steps required to move from current position to best signal`` (fileName: string, expected: int) =
  let result = HeightMap.parse fileName |> HeightMap.shortestStepsToGoal

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day12/sample.txt", -1)>]
[<InlineData("Day12/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = -1

  Assert.Equal(expected, result)
