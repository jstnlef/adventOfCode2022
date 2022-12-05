module Tests.Day5

open Xunit
open Day5

[<Theory>]
[<InlineData("Day5/testInput.txt", "CMZ")>]
[<InlineData("Day5/input.txt", "")>]
let ``Top crates after rearrangement`` (fileName: string, expected: string) =
  let result =
    parseSuppliesAndProcedures fileName
    ||> Supplies.rearrange
    |> Supplies.topCratesPerStack
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day5/testInput.txt", 0)>]
// [<InlineData("Day5/input.txt", -1)>]
let ``2`` (fileName: string, expected: int) =
  // let result =
  //   parseAssignmentPairs fileName
  //   |> countAssignments Pair.assignmentsAreOverlapping
  let result = -1
  Assert.Equal(expected, result)
