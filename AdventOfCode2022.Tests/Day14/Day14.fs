module Tests.Day14

open Xunit

[<Theory>]
[<InlineData("Day14/sample.txt", 13)>]
[<InlineData("Day14/input.txt", -1)>]
let ``Part 1`` (fileName: string, expected: int) =
  let result = -1

  Assert.Equal(expected, result)

[<Theory>]
[<InlineData("Day14/sample.txt", -1)>]
[<InlineData("Day14/input.txt", -1)>]
let ``Part 2`` (fileName: string, expected: int) =
  let result = -1

  Assert.Equal(expected, result)
