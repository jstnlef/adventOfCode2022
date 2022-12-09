module Tests.Day9

open Xunit
open Day9

[<Theory>]
[<InlineData("Day9/sample.txt", 13)>]
[<InlineData("Day9/largerSample.txt", 88)>]
[<InlineData("Day9/input.txt", 5907)>]
let ``Number of positions the tail visits after simulating 2 knots`` (fileName: string, expected: int) =
  let result =
    RopeSim.parse fileName
    |> RopeSim.simulate 2
    |> (fun state -> state.positionsTailVisited.Count)

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day9/sample.txt", 1)>]
[<InlineData("Day9/largerSample.txt", 36)>]
[<InlineData("Day9/input.txt", -1)>]
let ``Number of positions the tail visits after simulating 10 knots`` (fileName: string, expected: int) =
  let result =
    RopeSim.parse fileName
    |> RopeSim.simulate 10
    |> (fun state -> state.positionsTailVisited.Count)

  Assert.Equal(expected, result)
