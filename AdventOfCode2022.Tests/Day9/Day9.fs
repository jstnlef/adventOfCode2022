module Tests.Day9

open Xunit
open Day9

[<Theory>]
[<InlineData("Day9/sample.txt", 13)>]
[<InlineData("Day9/input.txt", 5907)>]
let ``Number of positions the tail visits after simulating`` (fileName: string, expected: int) =
  let result =
    RopeSim.parse fileName
    |> RopeSim.simulate
    |> (fun state -> state.positionsTailVisited.Count)

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day9/sample.txt", 8)>]
[<InlineData("Day9/input.txt", 371200)>]
let ``Part 2`` (fileName: string, expected: int) =
  // let result =
  //   TreeHeightMap.parse fileName
  //   |> TreeHeightMap.calculateHighestScenicScore
  let result = -1
  Assert.Equal(expected, result)
