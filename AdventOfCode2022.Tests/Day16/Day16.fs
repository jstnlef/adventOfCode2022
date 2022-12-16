module Tests.Day16

open Xunit

[<Theory>]
[<InlineData("Day16/sample.txt", -1)>]
[<InlineData("Day16/input.txt", -1)>]
let ``Part 1`` (fileName: string, expected: int) =
  let result = - 1
  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day16/sample.txt", -1)>]
[<InlineData("Day16/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = -1
  Assert.Equal(expected, result)
