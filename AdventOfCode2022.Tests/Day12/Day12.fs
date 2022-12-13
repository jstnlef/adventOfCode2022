module Tests.Day12

open Day12
open Xunit

[<Theory>]
[<InlineData("Day12/sample.txt", 31)>]
[<InlineData("Day12/input.txt", 394)>]
let ``Fewest steps required to move from current position to best signal`` (fileName: string, expected: int) =
  let result = HeightMap.parse fileName |> HeightMap.shortestStepsToGoal

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day12/sample.txt", 29)>]
[<InlineData("Day12/input.txt", 388)>]
let ``Fewest steps required to move starting from any square with elevation a`` (fileName: string, expected: int) =
  let result = HeightMap.parse fileName |> HeightMap.shortestStepsFromAnyA

  Assert.Equal(expected, result)
