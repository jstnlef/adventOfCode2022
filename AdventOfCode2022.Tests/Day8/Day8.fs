module Tests.Day8

open Xunit
open Day8

[<Theory>]
[<InlineData("Day8/sample.txt", 21)>]
[<InlineData("Day8/input.txt", 1705)>]
let ``Number of visible trees from outside the grid`` (fileName: string, expected: int) =
  let result =
    TreeHeightMap.parse fileName
    |> TreeHeightMap.findVisibleTrees
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day8/sample.txt", -1)>]
[<InlineData("Day8/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  // let result =
  //   parseSuppliesAndProcedures fileName
  //   ||> Supplies.rearrange Step.performWithCrateMover9000
  //   |> Supplies.topCratesPerStack
  let result = -1
  Assert.Equal(expected, result)
