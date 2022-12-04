module Tests.Day4

open Xunit
open Day4

[<Theory>]
[<InlineData("Day4/testInput.txt", 2)>]
[<InlineData("Day4/input.txt", 507)>]
let ``Number of pairs with fully contained assignments`` (fileName: string, expected: int) =
  let result =
    parseAssignmentPairs fileName
    |> countAssignments Pair.assignmentsAreFullyContained
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day4/testInput.txt", 4)>]
[<InlineData("Day4/input.txt", 897)>]
let ``Number of pairs with overlapping assignments`` (fileName: string, expected: int) =
  let result =
    parseAssignmentPairs fileName
    |> countAssignments Pair.assignmentsAreOverlapping
  Assert.Equal(expected, result)
