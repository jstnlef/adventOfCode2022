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
[<InlineData("Day8/sample.txt", 8)>]
[<InlineData("Day8/input.txt", 371200)>]
let ``Calculate the highest scenic score`` (fileName: string, expected: int) =
  let result =
    TreeHeightMap.parse fileName
    |> TreeHeightMap.calculateHighestScenicScore
  Assert.Equal(expected, result)
